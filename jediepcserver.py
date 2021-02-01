#!/usr/bin/env python

"""
Jedi EPC server.

Copyright (C) 2012 Takafumi Arakaki

Author: Takafumi Arakaki <aka.tkf at gmail.com>

This file is NOT part of GNU Emacs.

Jedi EPC server is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Jedi EPC server is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Jedi EPC server.
If not, see <http://www.gnu.org/licenses/>.

"""

import argparse
import glob
import itertools
import logging
import logging.handlers
import os
import re
import site
import sys
import pkg_resources
from collections import namedtuple

import jedi
import jedi.api

import epc
import epc.server
import sexpdata

logger = logging.getLogger('jediepcserver')


parser = argparse.ArgumentParser(
    formatter_class=argparse.RawTextHelpFormatter,
    description=__doc__)
parser.add_argument(
    '--address', default='localhost')
parser.add_argument(
    '--port', default=0, type=int)
parser.add_argument(
    '--port-file', '-f', default='-', type=argparse.FileType('wt'),
    help='file to write port on.  default is stdout.')
parser.add_argument(
    '--sys-path', '-p', default=[], action='append',
    help='paths to be inserted at the top of `sys.path`.')
parser.add_argument(
    '--sys-path-append', default=[], action='append',
    help='paths to be appended at the end of `sys.path`.')
parser.add_argument(
    '--virtual-env', '-v', default=[], action='append',
    help='paths to be used as if VIRTUAL_ENV is set to it.')
parser.add_argument(
    '--log', help='Save server log to this file.')
parser.add_argument(
    '--log-level',
    choices=['CRITICAL', 'ERROR', 'WARN', 'INFO', 'DEBUG'],
    help='Logging level for log file.')
parser.add_argument(
    '--log-rotate-max-size', default=0, type=int,
    help='Rotate log file after it reaches this size',
)
parser.add_argument(
    '--log-rotate-max-count', default=3, type=int,
    help='Max number of log rotations before removal',
)
parser.add_argument(
    '--log-traceback', action='store_true', default=False,
    help='Include traceback in logging output.')
parser.add_argument(
    '--pdb', dest='debugger', const='pdb', action='store_const',
    help='start pdb when error occurs.')
parser.add_argument(
    '--ipdb', dest='debugger', const='ipdb', action='store_const',
    help='start ipdb when error occurs.')


PY3 = (sys.version_info[0] >= 3)
NEED_ENCODE = not PY3


LogSettings = namedtuple(
    'LogSettings',
    [
        'log_file',
        'log_level',
        'log_rotate_max_size',
        'log_rotate_max_count',
    ],
)


try:
    jedi.create_environment
except AttributeError:
    jedi_create_environment = None
else:
    _cached_jedi_environments = {}

    def jedi_create_environment(venv, safe=False):
        """Cache jedi environments to avoid startup cost."""
        try:
            return _cached_jedi_environments[venv]
        except KeyError:
            logger.info('Creating jedi environment: %s', venv)
            if venv is None:
                jedienv = jedi.api.environment.get_default_environment()
            else:
                jedienv = jedi.create_environment(venv, safe=safe)
            _cached_jedi_environments[venv] = jedienv
            return jedienv

jedi_script_wrapper = jedi.Script
JEDI_VERSION = pkg_resources.parse_version(jedi.__version__)
if JEDI_VERSION < pkg_resources.parse_version('0.16.0'):
    class JediScriptCompatWrapper:
        def __init__(self, code, path, **kwargs):
            self.source = code
            self.source_path = path
            self.kwargs = kwargs

        def complete(self, line, column):
            return jedi.Script(self.source,
                               line, column,
                               self.source_path,
                               **self.kwargs).completions()

        def get_signatures(self, line, column):
            return jedi.Script(self.source,
                               line, column,
                               self.source_path,
                               **self.kwargs).call_signatures()

        def goto(self, line, column):
            return jedi.Script(self.source,
                               line, column,
                               self.source_path,
                               **self.kwargs).goto_assignments()

        def get_references(self, line, column):
            return jedi.Script(self.source,
                               line, column,
                               self.source_path,
                               **self.kwargs).usages()

        def infer(self, line, column):
            return jedi.Script(self.source,
                               line, column,
                               self.source_path,
                               **self.kwargs).goto_definitions()

        def get_names(self):
            return jedi.api.names(self.source, self.source_path, **self.kwargs)

    jedi_script_wrapper = JediScriptCompatWrapper


def get_venv_sys_path(venv):
    if jedi_create_environment is not None:
        return jedi_create_environment(venv).get_sys_path()
    from jedi.evaluate.sys_path import get_venv_path
    return get_venv_path(venv)


class JediEPCHandler(object):
    def __init__(self, sys_path=None, virtual_envs=None, sys_path_append=None):
        self.script_kwargs = JediEPCHandler._get_script_path_kwargs(
            sys_path, virtual_envs, sys_path_append
        )

    def get_sys_path(self):
        environment = self.script_kwargs.get('environment')
        if environment is not None:
            return environment.get_sys_path()
        sys_path = self.script_kwargs.get('sys_path')
        if sys_path is not None:
            return sys_path
        return sys.path

    @staticmethod
    def _get_script_path_kwargs(sys_path, virtual_envs, sys_path_append):
        result = {}
        if jedi_create_environment:
            # Need to specify some environment explicitly to workaround
            # https://github.com/davidhalter/jedi/issues/1242. Otherwise jedi
            # will create a lot of child processes.
            if virtual_envs:
                primary_env, virtual_envs = virtual_envs[0], virtual_envs[1:]
                primary_env = path_expand_vars_and_user(primary_env)
            else:
                primary_env = None
            try:
                result['environment'] = jedi_create_environment(primary_env)
            except Exception:
                logger.warning(
                    'Cannot create environment for %r', primary_env, exc_info=1
                )
                if primary_env is not None:
                    result['environment'] = jedi_create_environment(None)

        if not sys_path and not virtual_envs and not sys_path_append:
            # No additional path customizations.
            return result

        # Either multiple environments or custom sys_path extensions are
        # specified, or jedi version doesn't support environments.
        final_sys_path = []
        sys_path = () if sys_path is None else sys_path
        final_sys_path.extend(path_expand_vars_and_user(p) for p in sys_path)
        for p in virtual_envs:
            final_sys_path.extend(get_venv_sys_path(path_expand_vars_and_user(p)))
        sys_path_append = () if sys_path_append is None else sys_path_append
        final_sys_path.extend(
            path_expand_vars_and_user(p) for p in sys_path_append
        )
        dupes = set()

        def not_seen_yet(val):
            if val in dupes:
                return False
            dupes.add(val)
            return True
        result['sys_path'] = [p for p in final_sys_path if not_seen_yet(p)]
        return result

    def jedi_script(self, source, source_path):
        if NEED_ENCODE:
            source = source.encode('utf-8')
            source_path = source_path and source_path.encode('utf-8')
        return jedi_script_wrapper(code=source, path=source_path, **self.script_kwargs)

    def complete(self, source, line, column, source_path):
        def _wrap_completion_result(comp):
            try:
                docstr = comp.docstring()
            except Exception:
                logger.warning(
                    "Cannot get docstring for completion %s", comp, exc_info=1
                )
                docstr = ""
            return dict(
                word=comp.name,
                doc=docstr,
                description=candidates_description(comp),
                symbol=candidate_symbol(comp),
            )
        return [
            _wrap_completion_result(comp)
            for comp in self.jedi_script(source, source_path).complete(line, column)
        ]

    def get_in_function_call(self, source, line, column, source_path):
        sig = self.jedi_script(source, source_path).get_signatures(line, column)
        call_def = sig[0] if sig else None

        if not call_def:
            return []

        return dict(
            # p.description should do the job.  But jedi-vim use replace.
            # So follow what jedi-vim does...
            params=[PARAM_PREFIX_RE.sub('', p.description).replace('\n', '')
                    for p in call_def.params],
            index=call_def.index,
            call_name=call_def.name,
        )

    def goto(self, source, line, column, source_path):
        definitions = self.jedi_script(source, source_path).goto(line, column)
        return [definition_to_short_dict(d) for d in definitions]

    def related_names(self, source, line, column, source_path):
        definitions = self.jedi_script(source, source_path).get_references(line, column)
        return [definition_to_short_dict(d) for d in definitions]

    def get_definition(self, source, line, column, source_path):
        definitions = self.jedi_script(source, source_path).infer(line, column)
        return [definition_to_dict(d) for d in definitions]

    def defined_names(self, source, source_path):
        top_level_names = [
            defn
            for defn in self.jedi_script(source, source_path).get_names()
            if defn.parent().type == 'module'
        ]
        return list(map(get_names_recursively, top_level_names))

    def get_jedi_version(self):
        return [dict(
            name=module.__name__,
            file=getattr(module, '__file__', []),
            version=get_module_version(module) or [],
        ) for module in [sys, jedi, epc, sexpdata]]


def candidate_symbol(comp):
    """
    Return a character representing completion type.

    :type comp: jedi.api.Completion
    :arg  comp: A completion object returned by `jedi.Script.complete`.

    """
    try:
        return comp.type[0].lower()
    except (AttributeError, TypeError):
        return '?'


def candidates_description(comp):
    """
    Return `comp.description` in an appropriate format.

    * Avoid return a string 'None'.
    * Strip off all newlines. This is required for using
      `comp.description` as candidate summary.

    """
    desc = comp.description
    return _WHITESPACES_RE.sub(' ', desc) if desc and desc != 'None' else ''
_WHITESPACES_RE = re.compile(r'\s+')


PARAM_PREFIX_RE = re.compile(r'^param\s+')
"""RE to strip unwanted "param " prefix returned by param.description."""


def definition_to_dict(d):
    return dict(
        doc=d.docstring(),
        description=d.description,
        line_nr=d.line,
        column=d.column,
        module_path=str(d.module_path),
        name=getattr(d, 'name', '?'),
        full_name=getattr(d, 'full_name', '?'),
        type=getattr(d, 'type', '?'),
    )

def definition_to_short_dict(d):
    return dict(
        column=d.column,
        line_nr=d.line,
        module_path=str(d.module_path) if d.module_path != '__builtin__' else '',
        module_name=d.module_name,
        description=d.description,
    )

def get_names_recursively(definition, parent=None):
    """
    Fetch interesting defined names in sub-scopes under `definition`.

    :type names: jedi.api_classes.Definition

    """
    d = definition_to_dict(definition)
    try:
        d['local_name'] = parent['local_name'] + '.' + d['name']
    except (AttributeError, TypeError):
        d['local_name'] = d['name']
    if definition.type == 'class':
        ds = definition.defined_names()
        return [d] + [get_names_recursively(c, d) for c in ds]
    else:
        return [d]


def get_module_version(module):
    notfound = object()
    for key in ['__version__', 'version']:
        version = getattr(module, key, notfound)
        if version is not notfound:
            return version
    try:
        from pkg_resources import get_distribution, DistributionNotFound
        try:
            return get_distribution(module.__name__).version
        except DistributionNotFound:
            pass
    except ImportError:
        pass


def path_expand_vars_and_user(p):
    return os.path.expandvars(os.path.expanduser(p))


def configure_logging(log_settings):
    """
    :type log_settings: LogSettings
    """
    if not log_settings.log_file:
        return

    fmter = logging.Formatter('%(asctime)s:' + logging.BASIC_FORMAT)
    if log_settings.log_rotate_max_size > 0:
        handler = logging.handlers.RotatingFileHandler(
            filename=log_settings.log_file,
            mode='w',
            maxBytes=log_settings.log_rotate_max_size,
            backupCount=log_settings.log_rotate_max_count,
        )
    else:
        handler = logging.FileHandler(filename=log_settings.log_file, mode='w')
    handler.setFormatter(fmter)
    if log_settings.log_level:
        logging.root.setLevel(log_settings.log_level.upper())
    logging.root.addHandler(handler)


def jedi_epc_server(
        address='localhost',
        port=0,
        port_file=sys.stdout,
        sys_path=[],
        virtual_env=[],
        sys_path_append=[],
        debugger=None,
        log_traceback=None,
):
    """Start EPC server.

    :type log_settings: LogSettings

    """
    logger.debug(
        'jedi_epc_server: sys_path=%r virtual_env=%r sys_path_append=%r',
        sys_path, virtual_env, sys_path_append,
    )

    if not virtual_env and os.getenv('VIRTUAL_ENV'):
        logger.debug(
            'Taking virtual env from VIRTUAL_ENV: %r',
            os.environ['VIRTUAL_ENV'],
        )
        virtual_env = [os.environ['VIRTUAL_ENV']]

    handler = JediEPCHandler(
        sys_path=sys_path,
        virtual_envs=virtual_env,
        sys_path_append=sys_path_append,
    )
    logger.debug(
        'Starting Jedi EPC server with the following sys.path: %r',
        handler.get_sys_path(),
    )
    server = epc.server.EPCServer((address, port))
    server.register_function(handler.complete)
    server.register_function(handler.get_in_function_call)
    server.register_function(handler.goto)
    server.register_function(handler.related_names)
    server.register_function(handler.get_definition)
    server.register_function(handler.defined_names)
    server.register_function(handler.get_jedi_version)

    @server.register_function
    def toggle_log_traceback():
        server.log_traceback = not server.log_traceback
        return server.log_traceback

    port_file.write(str(server.server_address[1]))  # needed for Emacs client
    port_file.write("\n")
    port_file.flush()
    if port_file is not sys.stdout:
        port_file.close()

    # This is not supported Python-EPC API, but I am using this for
    # backward compatibility for Python-EPC < 0.0.4.  In the future,
    # it should be passed to the constructor.
    server.log_traceback = bool(log_traceback)

    if debugger:
        server.set_debugger(debugger)
        handler = logging.StreamHandler()
        fmter = logging.Formatter('%(asctime)s:' + logging.BASIC_FORMAT)
        handler.setFormatter(fmter)
        handler.setLevel(logging.DEBUG)
        server.logger.addHandler(handler)
        server.logger.setLevel(logging.DEBUG)
    return server


# def add_virtualenv_path(venv):
#     """Add virtualenv's site-packages to `sys.path`."""
#     venv = os.path.abspath(venv)
#     paths = glob.glob(os.path.join(
#         venv, 'lib', 'python*', 'site-packages'))
#     if not paths:
#         raise ValueError('Invalid venv: no site-packages found: %s' % venv)
#     for path in paths:
#         site.addsitedir(path)


def main(args=None):
    ns = parser.parse_args(args)

    ns_vars = vars(ns).copy()
    log_settings = LogSettings(
        log_file=ns_vars.pop('log'),
        log_level=ns_vars.pop('log_level'),
        log_rotate_max_size=ns_vars.pop('log_rotate_max_size'),
        log_rotate_max_count=ns_vars.pop('log_rotate_max_count'),
    )
    configure_logging(log_settings)
    server = jedi_epc_server(**ns_vars)
    server.serve_forever()
    server.logger.info('exit')


if __name__ == '__main__':
    main()
