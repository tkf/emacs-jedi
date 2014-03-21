try:
    from setuptools import setup
    args = {}
except ImportError:
    from distutils.core import setup
    args = dict(scripts=['jediepcserver.py'])

setup(
    name='jediepcserver',
    py_modules=['jediepcserver'],
    install_requires=[
        "jedi>=0.7.0",
        "epc>=0.0.4",
        "argparse",
    ],
    entry_points={
        'console_scripts': ['jediepcserver = jediepcserver:main'],
    },
    **args
)
