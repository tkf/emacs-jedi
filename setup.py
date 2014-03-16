try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name='jediepcserver',
    install_requires=[
        "jedi>=0.7.0",
        "epc>=0.0.4",
        "argparse",
    ],
    scripts=['jediepcserver.py'],
)
