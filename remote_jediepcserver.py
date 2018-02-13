import argparse
import shlex
import subprocess
import sys
import socket
from contextlib import closing
import tempfile


RUN_CMD = """
#!/bin/bash

DIR="${{HOME}}/.emacs.d/.python-environments/default"
if [ ! -d "${{DIR}}" ]; then
  virtualenv "${{DIR}}"
fi
source "${{DIR}}/bin/activate"
if ! command -v jediepcserver > /dev/null; then
  pip install --upgrade git+https://github.com/tkf/emacs-jedi
fi
jediepcserver --port={port} --log-level=DEBUG --log=/tmp/jedi-{port}.log &
echo $! > {pid_file}
"""


def main(args=None):
    parser = argparse.ArgumentParser()
    parser.add_argument('host')
    args = parser.parse_args()

    host = args.host

    # TODO: if port is taken on the remote server, we'll need to retry this
    port = find_free_port()
    pid_file = get_remote_tmp_file(host, 'jedi-epc-pid.XXXXXX')
    script = RUN_CMD.format(port=port, pid_file=pid_file)
    remote_script = copy_script_to_host(script, host)
    proc, forward_proc = start_remote_server(host, port, remote_script)
    try:
        proc.wait()
    except (KeyboardInterrupt, SystemExit):
        # TODO: kill the remote process
        subprocess.call(['ssh', host, 'pkill', '-F', pid_file])
        proc.wait()
        #forward_proc.kill()
        #forward_proc.wait()
        raise


def install_remotely(server):
    p = subprocess.call(['ssh', server, INSTALL_CMD])


def find_free_port():
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(('', 0))
        return s.getsockname()[1]


def start_remote_server(host, port, cmd):
    local_port = port #find_free_port()
    port_forward = 'localhost:{local_port}:localhost:{remote_port}'.format(
        local_port=local_port, remote_port=port)
    proc_cmd = [
        'ssh',
        '-F', '/dev/null',
        '-o', 'ExitOnForwardFailure yes',
        '-L', port_forward,
        host,
        cmd
    ]
    proc = subprocess.Popen(proc_cmd)
    return proc, None



def get_remote_tmp_file(host, template):
    temp_file = subprocess.check_output(
        ['ssh', host, 'mktemp', '-t', template]).decode('utf-8').strip()
    return temp_file


def copy_script_to_host(script, host):
    temp_file = get_remote_tmp_file(host, 'jedi-epc.XXXXXX')
    with tempfile.NamedTemporaryFile(mode='w') as f:
        f.write(script)
        f.flush()
        subprocess.call(
            ['scp', f.name, '{}:{}'.format(host, temp_file)],
            stdout=subprocess.DEVNULL)
        subprocess.call(['ssh', host, 'chmod +x {}'.format(temp_file)])
    return temp_file


if __name__ == '__main__':
    sys.exit(main())
