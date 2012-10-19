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


def jedi_epc_server(address='localhost', port=0):
    from epc.server import EPCServer
    server = EPCServer((address, port))
    server.register_function(complete)
    return server


def main(args=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    parser.add_argument(
        '--address', default='localhost')
    parser.add_argument(
        '--port', default=0, type=int)
    ns = parser.parse_args(args)

    server = jedi_epc_server(**vars(ns))
    server.print_port()  # needed for Emacs client

    server.serve_forever()
    server.logger.info('exit')


if __name__ == '__main__':
    main()
