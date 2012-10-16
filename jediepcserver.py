import jedi
import sexpdata


def complete(arg):
    (source, line, column, source_path) = arg
    source = source.value()
    source_path = source_path.value()
    script = jedi.Script(source, line, column, source_path)
    completions = []
    for comp in script.complete():
        completions.append(sexpdata.String(comp.word))
    return completions


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
