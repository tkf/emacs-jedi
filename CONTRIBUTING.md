# Contribution guide line


## Can I ask question in the issue tracker?

It is OK to ask question in the issue tracker, but if the question is
not too specific to Jedi.el, consider asking question in other place
such as [stackoverflow](http://stackoverflow.com).  For example,
please do not ask "How to install Jedi.el using package.el?" in the
issue tracker.  Also, please search older questions in the tracker.


## Bug report / Question

- Check [troubleshooting][troubleshooting] section first.

[troubleshooting]: http://tkf.github.io/emacs-jedi/latest/#troubleshooting


## Pull request

- After you submit a pull request, check the tests on Travis CI.
  If you find an error you can fix, please do it.
- Use PEP 8 style in Python file.
- Make sure that Emacs Lisp file compiles without *warning*
  (at least make sure you don't increase the number of warnings).
