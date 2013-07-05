# Contribution guide line


## Can I ask question in the issue tracker?

It is OK to ask question in the issue tracker, but if the question is
not too specific to Jedi.el, consider asking question in other place
such as [stackoverflow](http://stackoverflow.com).  For example,
please do not ask "How to install Jedi.el using package.el?" in the
issue tracker.  Also, please search older questions in the tracker.


## Bug report / Question

- Check [troubleshooting][troubleshooting] section first.
- Paste the result of [jedi:show-version-info][version-info] or include
  the version number of Emacs, Python, Jedi.el and dependencies of Jedi.el.
- Describe how you install Jedi.el and its Python dependencies.
- Check if you can reproduce the problem in a clean Emacs configuration
  launched by `make tryout`.

If you get something like `deferred error : (error ...)` in your echo
area, most of the time the error is from Jedi (Python library).  Get
traceback following [this instruction][traceback] and see where the
error is from.  If it is from Jedi, send the bug report to its [issue
tracker][jedi-issue].

[troubleshooting]: http://tkf.github.io/emacs-jedi/latest/#troubleshooting
[version-info]: http://tkf.github.io/emacs-jedi/latest/#jedi:show-version-info
[traceback]: http://tkf.github.io/emacs-jedi/latest/#how-to-get-traceback
[jedi-issue]: https://github.com/davidhalter/jedi/issues


## Pull request

- After you submit a pull request, check the tests on Travis CI.
  If you find an error you can fix, please do it.
- Use PEP 8 style in Python file.
- Make sure that Emacs Lisp file compiles without *warning*
  (at least make sure you don't increase the number of warnings).
