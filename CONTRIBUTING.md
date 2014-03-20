# Contribution guide line


## Where should I ask question about Jedi.el?

Please ask question in [StackOverflow with `emacs-jedi` tag][so-tag].

However, you may find some old questions in GitHub issue tracker so it
is worth searching in it.

[so-tag]: http://stackoverflow.com/questions/tagged/emacs-jedi


## Bug report / Question

- Check [troubleshooting][troubleshooting] section first.

[troubleshooting]: http://tkf.github.io/emacs-jedi/latest/#troubleshooting


## Pull request

- After you submit a pull request, check the tests on Travis CI.
  If you find an error you can fix, please do it.
- Use PEP 8 style in Python file.
- Make sure that Emacs Lisp file compiles without *warning*
  (at least make sure you don't increase the number of warnings).
