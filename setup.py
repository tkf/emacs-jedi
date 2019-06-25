try:
    from setuptools import setup
    args = {}
except ImportError:
    from distutils.core import setup
    args = dict(scripts=['jediepcserver.py'])
    print("""\
*** WARNING: setuptools is not found.  Using distutils...
It is highly recommended to install Jedi.el via M-x jedi:install-server.
Note: If you are using Windows, then Jedi.el will not work with distutils.
""")

setup(
    name='jediepcserver',
    version='0.2.7',
    py_modules=['jediepcserver', 'remote_jediepcserver'],
    install_requires=[
        "jedi>=0.11.0",
        "epc>=0.0.4",
        "argparse",
    ],
    entry_points={
        'console_scripts': [
            'jediepcserver = jediepcserver:main',
            'remote_jediepcserver = remote_jediepcserver:main'
        ],
    },
    **args
)
