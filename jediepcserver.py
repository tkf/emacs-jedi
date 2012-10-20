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

import jedi
from jedi import parsing
from jedi import evaluate


def candidate_symbol(comp):
    """
    Return a character representing completion type.

    :type comp: jedi.api.Completion
    :arg  comp: A completion object returned by `jedi.Script.complete`.

    .. TODO:: This function extremely depends on internal details.
       I need to fix this at some point.

    See also how `jedi.Completion.complete` is computed.

    """
    funcs = (parsing.Function, evaluate.Function)
    try:
        is_func = comp.name.parent().isinstance(funcs)
    except AttributeError:
        is_func = False
    if is_func:
        return 'f'
    if isinstance(comp.base, parsing.Module):
        return 'm'
    if isinstance(comp.base, parsing.Param):
        return '='
    return '?'


def candidates_description(comp):
    desc = comp.description
    return desc if desc and desc != 'None' else ''


def complete(source, line, column, source_path):
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
    script = jedi.Script(source, line, column, source_path or '')
    definitions = script.goto()
    if definitions:
        d = definitions[0]
        return dict(
            line_nr=d.line_nr,
            module_path=d.module_path,
        )
    else:
        return []  # nil


def jedi_epc_server(address='localhost', port=0, sys_path=[]):
    sys.path = sys_path + sys.path
    from epc.server import EPCServer
    server = EPCServer((address, port))
    server.register_function(complete)
    server.register_function(get_in_function_call)
    server.register_function(goto)
    return server


def add_virtualenv_path():
    """Add virtualenv's site-packages to `sys.path`."""
    venv = os.path.abspath(os.getenv('VIRTUAL_ENV'))
    if not venv:
        return
    path = os.path.join(
        venv, 'lib', 'python%d.%d' % sys.version_info[:2], 'site-packages')
    sys.path.insert(0, path)


def main(args=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    parser.add_argument(
        '--address', default='localhost')
    parser.add_argument(
        '--port', default=0, type=int)
    parser.add_argument(
        '--sys-path', '-p', default=[], action='append',
        help='paths to be inserted at the top of `sys.path`.')
    ns = parser.parse_args(args)

    add_virtualenv_path()
    server = jedi_epc_server(**vars(ns))
    server.print_port()  # needed for Emacs client

    server.serve_forever()
    server.logger.info('exit')


if __name__ == '__main__':
    main()
