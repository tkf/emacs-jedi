============================================
 Jedi.el - Python auto-completion for Emacs
============================================

.. sidebar:: Links:

   * `Documentation <http://tkf.github.com/emacs-jedi/>`_ (at GitHub Pages)

     * `Configuration <http://tkf.github.com/emacs-jedi#configuration>`_
     * `Command <http://tkf.github.com/emacs-jedi#command>`_
     * `Troubleshooting <http://tkf.github.com/emacs-jedi#troubleshooting>`_

   * `Repository <https://github.com/tkf/emacs-jedi>`_ (at GitHub)
   * `Issue tracker <https://github.com/tkf/emacs-jedi/issues>`_ (at GitHub)
   * `Travis CI <https://travis-ci.org/#!/tkf/emacs-jedi>`_ |build-status|


What is it?
===========

Jedi.el is a Python auto-completion package for Emacs.
It aims at helping your Python coding in a non-destructive way.
It also helps you to find information about Python objects, such as
docstring, function arguments and code location.

Jedi.el uses jedi_ (an awesome Python auto-completion library) and
EPC_ (an RPC stack for Emacs Lisp) and its `Python binding`_ to
communicate with Python process.  It also uses excellent Emacs
auto-complete_ module to start completion automatically.  As Jedi.el
always calls Python function asynchronously (thanks to EPC_), it will
not block your Emacs while your are editing.

.. _jedi: https://github.com/davidhalter/jedi
.. _EPC: https://github.com/kiwanami/emacs-epc
.. _Python binding: python-epc_
.. _python-epc: https://github.com/tkf/python-epc
.. _auto-complete: https://github.com/auto-complete/auto-complete


Quick start
===========

Install Jedi.el via el-get or MELPA (see install_ for more info) and
add this to your Emacs configuration::

  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)                      ; optional
  (setq jedi:complete-on-dot t)                 ; optional

If you install Jedi.el manually (BTW, you shouldn't!), you need to add
more stuff to it.  See `manual install`_ section.


Requirements
============

Emacs
-----
The following are the minimum requirements within Emacs to get Jedi.el up
and running. If you install Jedi.el via el-get, please move to the Python
dependencies.

EPC_
****
This package can be installed by MELPA or manually following the
instructions on the repo home page. The bare minimum configuration is
to have ``(require 'epc)`` somewhere in your init.el and/or load path.

deferred.el_ (> v0.3)
*********************
deferred.el does not require any special configuration and installs
automatically when EPC_ is installed.

auto-complete_
**************
Auto-complete is a powerful completion package and we shall not attempt to
replicate the documentation here. It is in your best interest to read the
documentation for configuration to suit your needs and system. The following
is a basic configuration to get you started::

    ;; Require
    (require 'auto-complete)
    ;; recommended configurations
    (require 'auto-complete-config)
    (ac-config-default)

    (setq
         ac-auto-start 2
         ac-override-local-map nil
         ac-use-menu-map t
         ac-set-trigger-key "TAB")
    ;; Default settings
    (define-key ac-menu-map "\C-n" 'ac-next)
    (define-key ac-menu-map "\C-p" 'ac-previous)

    ;; making it a bit faster
    (setq
         ac-delay 5
         ac-auto-show-menu 0.4
         ac-quick-help-delay 0.5)
    ;; using a dictionary
    (add-to-list 'ac-dictionary-directories"~/path/to/autocomplete/dict")

    (provide 'user-auto-complete)

- If your completion popup is broken when width of completion candidates is wide, try the newest version of popup.el_.

.. _deferred.el: https://github.com/kiwanami/emacs-deferred
.. _popup.el: https://github.com/auto-complete/popup-el

- Jedi.el is currently tested against Emacs 24.3-devel, 24.2 and 23.1.

Python
------
The following are the minimum requirements within your Python environment
to get Jedi.el up and running. Verify these have been installed with
``pip freeze`` in the virtualenv currently activated.

Jedi_
*****
Jedi.py is an awesome Python auto-completion module specifically built
to be awesomer than awesome. In other words, it works really well. Install
Jedi.py into the virtualenv you are working on or system wide: Docs_

.. _Docs: https://jedi.readthedocs.org/en/latest/docs/installation.html

python-epc_
***********
``pip install epc`` This can be system wide or per virtualenv as described
above.

argparse
********
If you are using Python 2.6 or earlier, please install argparse for completion
support.

- Jedi.el is tested against Python 2.6, 2.7 and 3.2.

Optional dependencies for automatic installation:
-------------------------------------------------
- virtualenv_
- make

.. _virtualenv: http://www.virtualenv.org


Install
=======

el-get
------

The easiest way to install Jedi.el is to use el-get_:
just do ``M-x el-get-install jedi``.
You need to have virtualenv_ to automatically install Python module
dependencies.  If your el-get does not have the recipes for Jedi.el
yet, get them from `this pull request`_.

.. _el-get: https://github.com/dimitri/el-get
.. _this pull request: https://github.com/dimitri/el-get/pull/927


package.el (MELPA)
------------------

You can install Jedi.el using package.el interface from MELPA_.  As
package.el does not support installing non-elisp packages, you need to
install Python part manually (see the next section).

.. _MELPA: http://melpa.milkbox.net

Manual install
--------------

1. Install EPC_ and auto-complete_.
2. Install Jedi.el.  Download the repository of Jedi.el and add it to
   `load-path`.
3. Install Jedi_ and python-epc_ by

   - ``make requirements`` (no need for root privileges [#]_) or
   - ``pip install -r requirements.txt`` if you want to determine
     where to install Python modules.  You need root privileges (i.e.,
     ``sudo``) to install it in system directory.

4. Add ``(autoload 'jedi:setup "jedi" nil t)`` in your Emacs configuration.

.. [#] You need virtualenv_ for ``make requirements``.  It installs
   all requirements for Jedi EPC server in an isolated Python
   environment in ``env/`` directory under the directory where jedi.el
   locates.  Note that you don't need to worry about if you want to
   use Jedi.el to complete modules in another virtualenv you made.
   Jedi EPC server recognize the virtualenv it is in (i.e., the
   environment variable ``VIRTUAL_ENV`` in your Emacs) and then add
   modules in that environment to its ``sys.path``.


Setup
=====

All you need to do is to call `jedi:setup` in python buffer.
To do that, add the following in your Emacs configuration::

   (add-hook 'python-mode-hook 'jedi:setup)

If auto-completion is all you need, use `jedi:ac-setup` instead::

   (add-hook 'python-mode-hook 'jedi:ac-setup)

To setup recommended keybinds for Jedi.el, add this to your Emacs
configuration.  Note that you must set `jedi:setup-keys` *before*
loading `jedi.el`.  See its docstring (``<f1> v jedi:setup-keys``) for
more information.::

   (setq jedi:setup-keys t)


Extension
=========

IPython integration
-------------------

Sometimes it is useful to find completion using Python interpreter.
To do that in a seamless manner, you can use IPython and its Emacs
binding EIN (Emacs IPython Notebook).  See ein:jedi-setup_ in the EIN
manual.  Using this setup, you can run auto-completion command in
Jedi.el and EIN simultaneously.

.. _ein:jedi-setup:
   http://tkf.github.com/emacs-ipython-notebook/#ein:jedi-setup


.. Build status badge
.. |build-status|
   image:: https://secure.travis-ci.org/tkf/emacs-jedi.png?branch=master
   :target: http://travis-ci.org/tkf/emacs-jedi
   :alt: Build Status
