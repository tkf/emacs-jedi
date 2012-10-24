import jedi
import epc
import sexpdata


def print_deps(module):
    print("""\
{name}.__file__ = {file}
{name}.__version__ = {version}
""".format(name=module.__name__,
           file=module.__file__,
           version=getattr(module, '__version__', None)))


print_deps(jedi)
print_deps(epc)
print_deps(sexpdata)
