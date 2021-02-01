import os
import subprocess
import sys
import textwrap
from contextlib import contextmanager

import jediepcserver as jep


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
    major_version, minor_version = sys.version_info[:2]
    envname = 'py{}{}'.format(major_version, minor_version)
    subprocess.check_call(['tox', '-e', envname, '--notest'])
    relative_venv_path = ".tox/" + envname
    full_venv_path = os.path.join(os.getcwd(), relative_venv_path)

    handler = jep.JediEPCHandler(virtual_envs=[full_venv_path])
    sys_path = handler.get_sys_path()
    venv_path = '{0}/lib/python{1}.{2}/site-packages'.format(
        full_venv_path, major_version, minor_version,
    )
    assert venv_path in sys_path


def check_defined_names(source, keys, deftree):
    stripdict = lambda d: dict((k, d[k]) for k in keys)
    striptree = lambda ds: [stripdict(ds[0])] + list(map(striptree, ds[1:]))
    handler = jep.JediEPCHandler()
    fulldicts = handler.defined_names(textwrap.dedent(source), 'example.py')
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


def _get_jedi_script_params(src, filename='example.py'):
    source = textwrap.dedent(src)
    lines = source.splitlines()
    return source, len(lines), len(lines[-1]), filename


def test_get_in_function_call():
    params = _get_jedi_script_params("""
    def foo(bar, baz, *qux, **quux):
        pass

    foo(
    """)
    handler = jep.JediEPCHandler()
    result = handler.get_in_function_call(*params)
    assert result == {
        'params': ['bar', 'baz', '*qux', '**quux'],
        'index': 0,
        'call_name': 'foo',
    }


def test_completion_docstring_raises(monkeypatch):
    """Test "complete" handler does not fail if "comp.docstring" raises

    Ref. #339
    """
    def raise_exception(*args, **kwargs):
        raise Exception('hello, world')

    monkeypatch.setattr(
        'jedi.api.classes.Completion.docstring', raise_exception
    )

    params = _get_jedi_script_params("""
    import os

    os.chd
    """)
    handler = jep.JediEPCHandler()
    result = handler.complete(*params)
    assert result == [
        {
            'word': 'chdir',
            'doc': '',
            'description': 'def chdir',
            'symbol': 'f',
        },
    ]
