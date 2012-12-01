import os
from contextlib import contextmanager

from jediepcserver import add_virtualenv_path


@contextmanager
def osenv(*args, **kwds):
    def putenvs(dct):
        for (k, v) in dct.items():
            if v is None:
                del os.environ[k]
            else:
                os.environ[k] = v
    newenv = dict(*args, **kwds)
    oldenv = dict(zip(newenv, map(os.getenv, newenv)))
    try:
        putenvs(newenv)
        yield
    finally:
        putenvs(oldenv)


def test_add_virtualenv_path_runs_fine_in_non_virtualenv():
    # See: https://github.com/tkf/emacs-jedi/issues/3
    with osenv(VIRTUAL_ENV=None):
        add_virtualenv_path()
