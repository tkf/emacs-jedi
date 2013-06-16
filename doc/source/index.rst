============================================
 Jedi.el - Python auto-completion for Emacs
============================================

.. sidebar:: Links:

   * `Documentation <http://tkf.github.io/emacs-jedi/>`_ (at GitHub Pages) [#]_

     * `Configuration`_
     * `Command`_
     * `Troubleshooting`_

   * `Repository <https://github.com/tkf/emacs-jedi>`_ (at GitHub)
   * `Issue tracker <https://github.com/tkf/emacs-jedi/issues>`_ (at GitHub)
   * `Travis CI <https://travis-ci.org/#!/tkf/emacs-jedi>`_ |build-status|

   .. [#] There are
     `released version <http://tkf.github.io/emacs-jedi/released>`_  and
     `developmental version <http://tkf.github.io/emacs-jedi/latest>`_.


What is it?
===========

Jedi.el is a Python auto-completion package for Emacs.
It aims at helping your Python coding in a non-destructive way.
It also helps you to find information about Python objects, such as
docstring, function arguments and code location.


Quick start
===========

(1) **Install** Jedi.el via el-get, Marmalade or MELPA (see install_ for
    more info).

(2) **Configure** Emacs using this::

      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:complete-on-dot t)                 ; optional

    If you install Jedi.el manually (BTW, you shouldn't!), you need to add
    more stuff to it.  See `manual install`_ section.

(3) (NOTE: el-get user can skip this step)
    **Setup Python requirements** in virtualenv by doing this in your shell::

      cd PATH/TO/JEDI
      make requirements

    Typically, ``PATH/TO/JEDI/`` is something like ``~/.emacs.d/elpa/jedi-*/``.
    If you prefer not using virtualenv, see install_ for more information.


Screenshots
===========

Jedi.el comes with a set of useful features.  Here is a list of screenshots
to show some of them.

.. figure:: http://farm9.staticflickr.com/8261/8804536872_8d266b88ed_o.png

   Auto-completion and popup help.  This is the main feature of Jedi.el.
   You don't need to type any special command.  Completions and help
   popup as you type.

.. figure:: http://farm3.staticflickr.com/2845/8793986161_e1c58607f0_o.png

   Popup-style call signature help.
   This is useful when you don't remember what argument to pass.

.. figure:: http://farm8.staticflickr.com/7312/8794015799_989e2a7217_o.png

   eldoc-style call signature help.
   This is another style of showing arguments.
   Use `jedi:tooltip-method` to configure which style to use.

.. figure:: http://farm4.staticflickr.com/3784/8804246558_0b3c998050_o.png

   Source code viewer (need jedi-direx_ extension).


Requirements
============

Emacs
-----
- EPC_
- deferred.el_ (> v0.3)
- auto-complete_

If your completion popup is broken when width of completion candidates
is wide, try the newest version of popup.el_.

.. _deferred.el: https://github.com/kiwanami/emacs-deferred
.. _popup.el: https://github.com/auto-complete/popup-el

Jedi.el is currently tested against Emacs 24.3-devel, 24.2 and 23.1.

Python
------
- Jedi_ (>= 0.6.0)
- python-epc_
- argparse (for Python 2.6)

Jedi.el is tested against Python 2.6, 2.7 and 3.2.

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


package.el (Marmalade or MELPA)
-------------------------------

You can install Jedi.el using package.el interface from Marmalade_ or
MELPA_.  As package.el does not support installing non-elisp packages,
you need to install Python part manually (see the next section).

.. _marmalade: http://marmalade-repo.org/packages/jedi
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


.. Links
.. _jedi: https://github.com/davidhalter/jedi
.. _EPC: https://github.com/kiwanami/emacs-epc
.. _Python binding: python-epc_
.. _python-epc: https://github.com/tkf/python-epc
.. _auto-complete: https://github.com/auto-complete/auto-complete
.. _jedi-direx: https://github.com/tkf/emacs-jedi-direx

.. Build status badge
.. |build-status|
   image:: https://secure.travis-ci.org/tkf/emacs-jedi.png?branch=master
   :target: http://travis-ci.org/tkf/emacs-jedi
   :alt: Build Status


.. ^- put some space after README.rst

Configuration
=============

.. el:package:: jedi

.. el:function:: jedi:setup
.. el:function:: jedi:ac-setup
.. el:variable:: jedi:complete-on-dot
.. el:variable:: jedi:server-command
   :value: '("python" "JEDI:SOURCE-DIR/jediepcserver.py")
.. el:variable:: jedi:server-args
.. el:variable:: jedi:get-in-function-call-timeout
.. el:variable:: jedi:get-in-function-call-delay
.. el:variable:: jedi:tooltip-method
.. el:variable:: jedi:goto-definition-config
.. el:variable:: jedi:goto-definition-marker-ring-length
.. el:variable:: jedi:doc-mode
.. el:variable:: jedi:doc-display-buffer
.. el:variable:: jedi:install-imenu
.. el:variable:: jedi:imenu-create-index-function


Keybinds
--------

.. el:keymap:: jedi-mode-map
.. el:variable:: jedi:use-shortcuts

.. el:variable:: jedi:setup-keys
.. el:variable:: jedi:key-complete
   :value: (kbd "<C-tab>")
.. el:variable:: jedi:key-goto-definition
   :value: (kbd "C-.")
.. el:variable:: jedi:key-show-doc
   :value: (kbd "C-c d")
.. el:variable:: jedi:key-related-names
   :value: (kbd "C-c r")
.. el:variable:: jedi:goto-definition-pop-marker
   :value: (kbd "C-,")


Command
=======

.. el:function:: jedi:stop-server
.. el:function:: jedi:start-dedicated-server
.. el:function:: jedi:complete
.. el:function:: jedi:get-in-function-call
.. el:function:: jedi:goto-definition
.. el:function:: jedi:goto-definition-pop-marker
.. el:function:: jedi:show-doc

.. el:package:: helm
.. el:function:: helm-jedi-related-names

.. el:package:: anything
.. el:function:: anything-jedi-related-names

.. el:package:: jedi
.. el:function:: jedi:pop-to-epc-buffer
.. el:function:: jedi:toggle-log-traceback
.. el:function:: jedi:toggle-debug-server
.. el:function:: jedi:show-version-info


Troubleshooting
===============

Before posting question or bug report in the `issue tracker`_, please
investigate the problem by yourself.  Here is some checklist.

#. You can try Jedi.el without installing it, by running
   ``make tryout`` if you have carton_ installed.  This will
   install requirements for Jedi.el separated from your local setup in
   ``.emacs.d``.  You can also check the configuration file
   tryout-jedi.el_ to see a minimum working configuration.  This is
   the configuration file loaded by ``make tryout``.  If you have
   trouble setting up Jedi.el, compare your configuration file and
   ``tryout-jedi.el``.

   If you get some error during ``make tryout`` or any other ``make``
   tasks, checking ``elpa/install.log`` may help you finding the
   problem.

   .. _carton: https://github.com/rejeep/carton
   .. _tryout-jedi.el:
      https://github.com/tkf/emacs-jedi/blob/master/tryout-jedi.el

   If you install carton_ in a different place or you don't add it to
   the ``$PATH``, you can call ``make`` like this:
   ``make CARTON=PATH/TO/bin/carton tryout``.
   Typically, ``PATH/TO/bin/carton`` is ``~/.carton/bin/carton``.

   If you are too lazy to go to carton_ site to checkout how to
   install it, here is what you need to do::

     curl -fsSkL https://raw.github.com/rejeep/carton/master/go | sh
     make CARTON=$HOME/.carton/bin/carton tryout

   Note that this carton_ is different from `the one for Perl
   <https://github.com/miyagawa/carton>`_.

#. To make sure that jedi.el is running correctly, you can
   do ``M-x jedi:show-jedi-version``.  It will show the versions
   of the Python libraries you are using.

   This is least complex way to communicate with the Jedi server.  If
   it doesn't work, rest of Jedi.el functions will not work.

#. To check that :el:symbol:`jedi:setup` is called properly via
   :el:symbol:`python-mode-hook`, run ``M-: jedi-mode RET`` in
   some Python file.  It should return `t`.

#. If you see message "auto-complete-mode is not enabled", you might
   forget to setup auto-complete properly.  Calling
   ``(global-auto-complete-mode t)`` in your Emacs configuration
   after *loading* auto-complete should solve this problem.

   "After loading" means you need to call ``(require 'auto-complete)``
   (or ``(require 'auto-complete-config)`` if you need) before
   calling ``(global-auto-complete-mode t)``.

#. It is possible that Jedi's keybind conflicts with keybinds
   of other libraries.  You can check the definition of keybind
   by ``<f1> k C-c ?`` (or ``C-h`` instead of ``<f1>``), for example.
   This one should show the help for :el:symbol:`jedi:show-doc`.

#. Make sure you are reading right version of document.  If you
   are using developmental version (installed via el-get, MELPA
   or manually from github), you should read `developmental version
   <http://tkf.github.io/emacs-jedi/latest>`_.  If you installed
   from Marmalade, you should read `released version
   <http://tkf.github.io/emacs-jedi/released>`_.


FAQ
===

How to update Python dependencies
---------------------------------

.. note:: el-get user can just use ``M-x el-get-update RET jedi RET``
   to update Emacs Lisp *and* Python dependencies.

Simply run::

  cd PATH/TO/JEDI
  make requirements

Or, if you install dependencies to somewhere else (i.e., you did not
use the ``make`` command when you installed.), use ``pip`` directly.
You may need ``sudo`` depending on the location you installed
dependencies.::

  pip install -r PATH/TO/requirements.txt


How to get traceback
--------------------

You need to toggle on traceback logging for EPC server and then
run :el:symbol:`jedi:pop-to-epc-buffer` when you get an error.
To start traceback logging, there are several options.

1. If server-client communication works (i.e., some completion or
   command work), use :el:symbol:`jedi:toggle-log-traceback`.

2. Alternatively, you can start server with ``--log-traceback`` option
   by :el:symbol:`jedi:start-dedicated-server`.  Run command by
   ``M-x jedi:start-dedicated-server RET`` and append ``--log-traceback``
   to the default command.

3. You can use :el:symbol:`jedi:server-args` to turn on logging always.
   This could be useful when you don't know when the error occurs.::

     (setq jedi:server-args '("--log-traceback"))


How it works
============

Jedi.el uses jedi_ (an awesome Python auto-completion library) and
EPC_ (an RPC stack for Emacs Lisp) and its `Python binding`_ to
communicate with Python process.  It also uses excellent Emacs
auto-complete_ module to start completion automatically.  As Jedi.el
always calls Python function asynchronously (thanks to EPC_), it will
not block your Emacs while your are editing.

  .. figure:: img/how-it-works.*

     How Jedi.el works.  Jedi.el calls Python methods in jedi_ through
     EPC_ protocol.  Emacs side implementation of EPC is `epc.el <EPC>`_
     and Python side is python-epc_.  Message through socket is encoded
     using S-expression based protocol.  See `EPC README file <EPC>`_
     for more details.

EPC_ is built on top of asynchronous library called deferred.el_.
One of the design goal for Jedi.el is to **never block user**.
This is extremely important thing to have in mind because you use
Emacs to edit something, not to wait some random lisp program to finish.
EPC_ and deferred.el_ are perfect libraries to achieve this goal.


Changelog
=========

v0.1.3 (WIP)
------------

Highlights:

- Add :el:symbol:`jedi:toggle-log-traceback` and
  :el:symbol:`jedi:pop-to-epc-buffer`.
- Add default keybind and a simple way to setup recommended keybinds.
  See issue `#47`_ for the reason and discussion behind this change.
- Now :el:symbol:`jedi:ac-setup` auto-magically enables auto-complete-mode.
  This is to help people from setting up auto-complete when they were not
  using it before installing jedi.el.  See:
  `#40 <https://github.com/tkf/emacs-jedi/issues/40>`_,
  `bbatsov/prelude#251 <https://github.com/bbatsov/prelude/issues/251>`_,
  `stackoverflow <http://stackoverflow.com/questions/15658963>`_.

Contributions from:

.. include:: releases/v0.1.3/authors.txt

Closed issues and pulled patches:

   .. include:: releases/v0.1.3/closed.txt

v0.1.2 (2013-05-26)
-------------------

Highlights:

- Package is available from Marmalade_.
- Add imenu support (see :el:symbol:`jedi:install-imenu` and
  :el:symbol:`jedi:imenu-create-index-function`).
  Currently it is not on by default as it needs developmental version
  of Jedi_.
- Add :el:symbol:`jedi:goto-definition-config` configurable option.
- Jedi.el now pools server instances.
  So, for example, you can create buffer-local :el:symbol:`jedi:server-args`
  to setup project-specific Jedi server (issue-28_).
- Do not expand common part when completing on inserting dot using
  :el:symbol:`jedi:dot-complete`.
- Strip off newlines from candidate summary.  This prevents
  popup to be disrupted when showing candidates summary
  containing newlines (e.g., ``json.__all__``).

Contributions from:

.. include:: releases/v0.1.2/authors.txt

Closed issues and pulled patches:

   .. include:: releases/v0.1.2/closed.txt

.. _marmalade: http://marmalade-repo.org/packages/jedi
.. _issue-28: https://github.com/tkf/emacs-jedi/issues/28

v0.1.1 (2012-12-01)
-------------------

- Add experimental "full-name" support [#fullname]_.
- PR-11_ fixes Makefile for BSD make (thanks, `@goro1080`_!).
- Fix issue-9_: line number sent to the server was shifted when the cursor
  is at the beginning of line.
- Fix issue-10_: get_in_function_call was called in non-python-mode buffer.
- Fix issue-7_: server process was killed unexpectedly.
- Add :el:symbol:`jedi:setup-keys`.  You don't need to manually add
  Jedi commands to :el:symbol:`python-mode-map` now.

Contributions from:

.. include:: releases/v0.1.1/authors.txt

Closed issues and pulled patches:

   .. include:: releases/v0.1.1/closed.txt

.. _@goro1080: https://github.com/goro1080

.. _PR-11: https://github.com/tkf/emacs-jedi/pull/11
.. _issue-10: https://github.com/tkf/emacs-jedi/issues/10
.. _issue-9: https://github.com/tkf/emacs-jedi/issues/9
.. _issue-7: https://github.com/tkf/emacs-jedi/issues/7

.. [#fullname] `jedi:get-full-name-*` functions require developmental
   version of Jedi_.
   See also: `Request: Definition.fullname · Issue #61 ·
   davidhalter/jedi <https://github.com/davidhalter/jedi/issues/61>`_

v0.1.0 (2012-11-09)
-------------------

- PR-8_ adds ELDoc like argument highlighting (thanks, `@syohex`_!).
- PR-2_ adds meta-data in header comment for ELPA (thanks, `@syohex`_!).
- PR-1_ fixes Makefile for newer pip version (thanks, `@L42y`_!).
- First version.

Contributions from:

.. include:: releases/v0.1.0/authors.txt

Closed issues and pulled patches:

   .. include:: releases/v0.1.0/closed.txt

.. _PR-8: https://github.com/tkf/emacs-jedi/pull/8
.. _PR-2: https://github.com/tkf/emacs-jedi/pull/2
.. _PR-1: https://github.com/tkf/emacs-jedi/pull/1

.. _@syohex: https://github.com/syohex
.. _@L42y: https://github.com/L42y


.. authors-to-github account map:

.. _Aaron Meurer: https://github.com/asmeurer
.. _Danilo Bargen: https://github.com/dbrgn
.. _Fabián Ezequiel Gallina: https://github.com/fgallina
.. _immerrr: https://github.com/immerrr
.. _Jaakko Pallari: https://github.com/jkpl
.. _Kiyono Goro: https://github.com/goro1080
.. _L42y: https://github.com/L42y
.. _Ryan Olf: https://github.com/ryanolf
.. _Syohei YOSHIDA: https://github.com/syohex
