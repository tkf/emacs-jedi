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

import os
import sys
import itertools

jedi = None  # I will load it later


PY3 = (sys.version_info[0] >= 3)
NEED_ENCODE = not PY3


def candidate_symbol(comp):
    """
    Return a character representing completion type.

    :type comp: jedi.api.Completion
    :arg  comp: A completion object returned by `jedi.Script.complete`.

    .. TODO:: This function extremely depends on internal details.
       I need to fix this at some point.

    See also how `jedi.Completion.complete` is computed.

    """
    def isit(what):
        try:
            return comp.name.parent().isinstance(what)
        except AttributeError:
            return False
    if isit((jedi.parsing.Function, jedi.evaluate.Function)):
        return 'f'
    if isit((jedi.parsing.Import)):
        return 'm'
    if isit((jedi.parsing.Class, jedi.evaluate.Class)):
        return 'c'
    if isinstance(comp.base, jedi.parsing.Module):
        return 'm'
    if isinstance(comp.base, jedi.parsing.Param):
        return '='
    return '?'


def candidates_description(comp):
    desc = comp.description
    return desc if desc and desc != 'None' else ''


def complete(source, line, column, source_path):
    if NEED_ENCODE:
        source = source.encode()
        source_path = source_path and source_path.encode()
    script = jedi.Script(source, line, column, source_path or '')
    reply = []
    for comp in script.complete():
        reply.append(dict(
            word=comp.word,
            doc=comp.doc,
            description=candidates_description(comp),
            symbol=candidate_symbol(comp),
        ))
    return reply


def get_in_function_call(source, line, column, source_path):
    if NEED_ENCODE:
        source = source.encode()
        source_path = source_path and source_path.encode()
    script = jedi.Script(source, line, column, source_path or '')
    call_def = script.get_in_function_call()
    if call_def:
        return dict(
            # p.get_code(False) should do the job.  But jedi-vim use replace.
            # So follow what jedi-vim does...
            params=[p.get_code().replace('\n', '') for p in call_def.params],
            index=call_def.index,
            call_name=call_def.call_name,
        )
    else:
        return []  # nil


def goto(source, line, column, source_path):
    if NEED_ENCODE:
        source = source.encode()
        source_path = source_path and source_path.encode()
    script = jedi.Script(source, line, column, source_path or '')
    definitions = script.goto()
    return [dict(
        line_nr=d.line_nr,
        module_path=d.module_path,
    ) for d in definitions]


def related_names(source, line, column, source_path):
    if NEED_ENCODE:
        source = source.encode()
        source_path = source_path and source_path.encode()
    script = jedi.Script(source, line, column, source_path or '')
    definitions = script.related_names()
    return [dict(
        column=d.column,
        line_nr=d.line_nr,
        module_path=d.module_path,
        module_name=d.module_name,
        description=d.description,
    ) for d in definitions]


def get_definition(source, line, column, source_path):
    if NEED_ENCODE:
        source = source.encode()
        source_path = source_path and source_path.encode()
    script = jedi.Script(source, line, column, source_path or '')
    definitions = script.get_definition()
    return [dict(
        doc=d.doc,
        desc_with_module=d.desc_with_module,
        line_nr=d.line_nr,
        module_path=d.module_path,
    ) for d in definitions]


def get_jedi_version():
    import epc
    import sexpdata
    try:
        from pkg_resources import get_distribution
        get_version = lambda x: get_distribution(x.__name__).version
    except ImportError:
        get_version = lambda x: getattr(x, '__version__', [])  # null
    return [dict(
        name=module.__name__,
        file=module.__file__,
        version=get_version(module),
    ) for module in [jedi, epc, sexpdata]]


def jedi_epc_server(address='localhost', port=0, port_file=sys.stdout,
                    sys_path=[], debugger=None):
    add_virtualenv_path()
    sys_path = map(os.path.expandvars, map(os.path.expanduser, sys_path))
    sys.path = [''] + list(filter(None, itertools.chain(sys_path, sys.path)))
    # Workaround Jedi's module cache.  Use this workaround until Jedi
    # got an API to set module paths.
    # See also: https://github.com/davidhalter/jedi/issues/36
    import_jedi()
    import epc.server
    server = epc.server.EPCServer((address, port))
    server.register_function(complete)
    server.register_function(get_in_function_call)
    server.register_function(goto)
    server.register_function(related_names)
    server.register_function(get_definition)
    server.register_function(get_jedi_version)

    port_file.write(str(server.server_address[1]))  # needed for Emacs client
    port_file.write("\n")
    port_file.flush()
    if port_file is not sys.stdout:
        port_file.close()

    if debugger:
        import logging
        epc.server.setuplogfile()
        server.logger.setLevel(logging.DEBUG)
        server.set_debugger(debugger)

    server.serve_forever()
    server.logger.info('exit')
    return server


def import_jedi():
    global jedi
    import jedi
    import jedi.parsing
    import jedi.evaluate
    return jedi


def add_virtualenv_path():
    """Add virtualenv's site-packages to `sys.path`."""
    venv = os.getenv('VIRTUAL_ENV')
    if not venv:
        return
    venv = os.path.abspath(venv)
    path = os.path.join(
        venv, 'lib', 'python%d.%d' % sys.version_info[:2], 'site-packages')
    sys.path.insert(0, path)


def main(args=None):
    import argparse
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
        '--pdb', dest='debugger', const='pdb', action='store_const',
        help='start pdb when error occurs.')
    parser.add_argument(
        '--ipdb', dest='debugger', const='ipdb', action='store_const',
        help='start ipdb when error occurs.')
    ns = parser.parse_args(args)
    jedi_epc_server(**vars(ns))


if __name__ == '__main__':
    main()
