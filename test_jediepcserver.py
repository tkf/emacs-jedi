import os
import textwrap
from contextlib import contextmanager

import jediepcserver as jep
jep.import_jedi()


@contextmanager
def osenv(*args, **kwds):
    def putenvs(dct):
        for (k, v) in dct.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v
    newenv = dict(*args, **kwds)
    oldenv = dict(zip(newenv, map(os.getenv, newenv)))
    try:
        putenvs(newenv)
        yield
    finally:
        putenvs(oldenv)


def test_epc_server_runs_fine_in_non_virtualenv():
    # See: https://github.com/tkf/emacs-jedi/issues/3
    with osenv(VIRTUAL_ENV=None):
        jep.jedi_epc_server()


def test_epc_server_runs_fine_in_virtualenv():
    import sys
    major_version = sys.version_info[:2][0]
    minor_version = sys.version_info[:2][1]
    relative_venv_path = ".tox/py"
    full_venv_path = os.path.join(os.getcwd(), relative_venv_path)

    with osenv(VIRTUAL_ENV=full_venv_path):
        jep.jedi_epc_server()

    venv_path = '{0}/lib/python{1}.{2}/site-packages'.format(full_venv_path,
                                                             major_version,
                                                             minor_version)
    assert venv_path in sys.path


def check_defined_names(source, keys, deftree):
    stripdict = lambda d: dict((k, d[k]) for k in keys)
    striptree = lambda ds: [stripdict(ds[0])] + list(map(striptree, ds[1:]))
    fulldicts = jep.defined_names(textwrap.dedent(source), 'example.py')
    dicts = list(map(striptree, fulldicts))
    assert dicts == deftree


def test_defined_names_imports():
    item = lambda name, local_name: {'name': name, 'local_name': local_name}
    keys = ['name', 'local_name']
    dicts = [
        [item('f', 'f')],
        [item('g', 'g')],
        [item('C', 'C'), [item('h', 'C.h')]],
    ]
    check_defined_names("""
    from module import f
    g = f(f)
    class C:
        h = g
    """, keys, dicts)


def test_defined_names_nested_classes():
    item = lambda name, local_name: {'name': name, 'local_name': local_name}
    keys = ['name', 'local_name']
    dicts = [
        [item('L1', 'L1'),
         [item('L2', 'L1.L2'),
          [item('L3', 'L1.L2.L3'),
           [item('f', 'L1.L2.L3.f')]],
          [item('f', 'L1.L2.f')]],
         [item('f', 'L1.f')]],
        [item('f', 'f')]]
    check_defined_names("""
    class L1:
        class L2:
            class L3:
                def f(): pass
            def f(): pass
        def f(): pass
    def f(): pass
    """, keys, dicts)
