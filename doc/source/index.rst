============================================
 Jedi.el - Python auto-completion for Emacs
============================================

.. sidebar:: Links:

   * `Documentation <http://tkf.github.io/emacs-jedi/>`_ (at GitHub Pages) [#]_

     * `Configuration`_
     * `Command`_
     * `Troubleshooting`_
     * `FAQ`_
     * :doc:`Complete overview <contents>`
     * :doc:`changelog`

   * `Q & A in StackOverflow
     <http://stackoverflow.com/questions/tagged/emacs-jedi>`_
     (with ``emacs-jedi`` tag)
   * `Repository <https://github.com/tkf/emacs-jedi>`_ (at GitHub)
   * `Issue tracker <https://github.com/tkf/emacs-jedi/issues>`_ (at GitHub)
   * `Travis CI <https://travis-ci.org/tkf/emacs-jedi>`_ |build-status|

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
    more info), i.e., either

    - ``M-x el-get-install RET jedi RET`` or
    - ``M-x package-install RET jedi RET`` or
    - (manually install...)

(2) **Configure** Emacs using this::

      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:complete-on-dot t)                 ; optional

    If you install Jedi.el manually (BTW, you shouldn't!), you need to add
    more stuff to it.  See `manual install`_ section.

(3) **Install Python server** (jediepcserver.py) by running

    - ``M-x jedi:install-server`` in Emacs

    (see also :el:symbol:`jedi:install-server`).

See install_ section for minimal examples.


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
- python-environment.el_

If your completion popup is broken when width of completion candidates
is wide, try the newest version of popup.el_.

.. _deferred.el: https://github.com/kiwanami/emacs-deferred
.. _popup.el: https://github.com/auto-complete/popup-el
.. _python-environment.el: https://github.com/tkf/emacs-python-environment

Jedi.el is currently tested against Emacs 24.3-devel, 24.2 and 23.1.

Command line program
--------------------

- virtualenv_

.. _virtualenv: http://www.virtualenv.org

Python
------
- Jedi_ (>= 0.6.0)
- python-epc_
- argparse (for Python 2.6)

Jedi.el is tested against Python 2.6, 2.7, 3.2 and 3.3.


Install
=======

el-get
------

If you have el-get_ installed, Jedi.el can be installed by these Emacs
command:

- ``M-x el-get-install RET jedi RET``
- ``M-x jedi:install-server RET`` (see also :ref:`pyinstall`)

Note that Python packages are *not* installed automatically anymore
(there is `a plan <https://github.com/tkf/emacs-jedi/issues/134>`_ to
fix it).

Here is a minimal example to setup Jedi.el via el-get_:

.. literalinclude:: jedi-el-get.el
   :language: cl

See also: :ref:`minimal-tryout`.

.. _el-get: https://github.com/dimitri/el-get


package.el (Marmalade or MELPA)
-------------------------------

You can install Jedi.el using package.el interface.
You need to add Marmalade_ or MELPA_ to :el:symbol:`package-archives`.

After you setup :el:symbol:`package-archives` properly, Jedi.el can be
installed by these Emacs command:

- ``M-x package-install RET jedi RET``
- ``M-x jedi:install-server RET`` (see also :ref:`pyinstall`)

Here is a minimal example to setup Jedi.el via package.el + MELPA:

.. literalinclude:: jedi-melpa.el
   :language: cl

.. Un-comment the following once v0.2.0 is released in Marmalade:

   Here is a minimal example to setup Jedi.el via package.el + Marmalade.
   The only difference with the above MELPA case is the value of
   :el:symbol:`package-archives`:

   .. literalinclude:: jedi-marmalade.el
      :language: cl

See also: :ref:`minimal-tryout`.

.. _Marmalade: http://marmalade-repo.org
.. _MELPA: http://melpa.milkbox.net

Manual install
--------------

1. Install EPC_ and auto-complete_.
2. Install Jedi.el.  Download the repository of Jedi.el and add it to
   `load-path`.
3. Add ``(autoload 'jedi:setup "jedi" nil t)`` in your Emacs configuration.
4. Run ``M-x jedi:install-server RET`` (see also :ref:`pyinstall`)

.. _pyinstall:

Python server (jediepcserver.py) installation
---------------------------------------------

As of Jedi.el v0.2.0, jediepcserver.py installation is done by running
Emacs command :el:symbol:`jedi:install-server`, i.e., typing
``M-x jedi:install-server RET`` in Emacs.  The same command can be used to
update Python packages used by Jedi.el.  So, running this command
after updating Jedi.el each time is recommended.

You can configure the location of the Python packages installed by
:el:symbol:`jedi:install-server` by changing the following variables:

- :el:symbol:`jedi:environment-root`
- :el:symbol:`python-environment-directory`
- :el:symbol:`python-environment-default-root-name`

If you want to install Python packages manually, rather than using
:el:symbol:`jedi:install-server`, see :ref:`manual-pyinstall` below.

.. _manual-pyinstall:

Manually install Python server (jediepcserver.py)
-------------------------------------------------

Install jediepcserver.py script to wherever you want.  For example,
you can use...

- pip::

    pip install -U PATH/TO/EMACS-JEDI/

- setup.py directly::

    cd PATH/TO/EMACS-JEDI/
    python setup.py install

Then find where your jediepcserver.py ends up.  Set the location by::

    (setq jedi:server-command '("PATH/TO/jediepcserver.py"))

See also :el:symbol:`jedi:server-command`.


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


Configuration
=============

.. el:package:: jedi

.. el:function:: jedi:setup
.. el:function:: jedi:ac-setup
.. el:variable:: jedi:complete-on-dot
.. el:variable:: jedi:environment-root
.. el:variable:: jedi:environment-virtualenv
.. el:variable:: jedi:server-command
   :value: '("~/.emacs.d/.python-environments/default/bin/jediepcserver.py")
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
.. el:variable:: jedi:install-python-jedi-dev-command


Keybinds
--------

.. el:keymap:: jedi-mode-map
.. el:variable:: jedi:use-shortcuts


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
.. el:function:: jedi:install-server
.. el:function:: jedi:install-python-jedi-dev
.. el:function:: jedi:pop-to-epc-buffer
.. el:function:: jedi:toggle-log-traceback
.. el:function:: jedi:toggle-debug-server
.. el:function:: jedi:show-setup-info
.. el:function:: jedi:show-version-info


Troubleshooting
===============

.. note:: If you have a question, ask question in StackOverflow_ with
   ``emacs-jedi`` tag.  We stopped using the issue tracker for Q & A.

Before posting question in StackOverflow_ or bug report in the `issue
tracker`_, please investigate the problem by yourself.  Here is some
checklist.

It is best to mention that you went through this list (or stuck with
somewhere) in your question or bug report.

.. _StackOverflow: http://stackoverflow.com/questions/tagged/emacs-jedi

#. Run :kbd:`M-x` :el:symbol:`jedi:show-setup-info` :kbd:`RET` (if you
   don't have it, try :el:symbol:`jedi:show-version-info` instead).
   This helps you in two ways.

   1. Paste the result of this function when you are asking question
      or reporting bug, to give people basic information.

      Unless you are sure that it is irrelevant, it is recommended to
      put this information.  If you think it is too long for a simple
      question/report, you can always use https://gist.github.com etc.

   2. To make sure jedi.el is running correctly and communicating with
      Jedi EPC server.

      This is the least complex way to communicate with the Jedi
      server.  If it doesn't work, rest of Jedi.el functions will not
      work.

#. You can try Jedi.el without installing it, in a "clean Emacs setup"
   [#]_ .  If you don't know what is wrong and don't know what to do
   next, this it the best thing to try.  This helps you to find
   out if your setting is wrong or if it is actually a bug in Jedi.el.

   Check :ref:`quick-try`.  If it works, compare with your Emacs setup
   carefully.  It is likely that there is something wrong in your
   Emacs setup.  You should also check minimal working examples in the
   install_ section.

   (Note that currently there is no automated way to do this in
   Windows without Cygwin.  If you know it, please document!)

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

#. If you get something like ``deferred error : (error ...)`` in your
   echo area (equivalently, the ``*Messages*`` buffer), most of the
   time the error is from Python-Jedi.  Get traceback following "`How
   to get traceback`_" and see where the error is from.  If it is from
   Python-Jedi, send the bug report to its `issue tracker`__.

   __ https://github.com/davidhalter/jedi/issues

#. Make sure you are reading right version of document.  If you
   are using developmental version (installed via el-get, MELPA
   or manually from github), you should read `developmental version
   <http://tkf.github.io/emacs-jedi/latest>`_.  If you installed
   from Marmalade, you should read `released version
   <http://tkf.github.io/emacs-jedi/released>`_.


.. [#] By "clean Emacs setup" I mean a Emacs process started in such a
   way that it does not read your Emacs configuration file and
   libraries installed by you.  In this manual, several ways to do
   that are described.  See :ref:`quick-try`.


FAQ
===

.. _quick-try:

How to quickly try Jedi.el without installing it
------------------------------------------------

There are two ways.  One for new users and one for Jedi.el developers.

1. :ref:`minimal-tryout`
2. :ref:`make-tryout`


.. _minimal-tryout:

Use minimal example setting to try Jedi.el without installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is recommended if you are new to Jedi.el.

1. Try Jedi.el without installation using package.el + MELPA

   Here is how to try el-get setup without touching your
   ``~/.emacs.d/``.  This version uses package.el and MELPA.  Once
   emacs is launched, type ``M-x package-install RET jedi RET`` and
   ``M-x jedi:install-server RET``:

   .. literalinclude:: jedi-melpa.sh
      :language: sh

2. Try Jedi.el without installation using el-get

   If you want to try el-get setup, do the following instead.  You may
   remove ``--eval "(setq el-get-install-skip-emacswiki-recipes nil)"``
   like the last commented line, but it will be very slow to start Emacs.
   Once emacs is launched, type ``M-x el-get-install RET jedi RET`` and
   ``M-x jedi:install-server RET``

   .. literalinclude:: jedi-el-get.sh
      :language: sh


The above methods run minimal example mentioned in the install_
section.


.. note:: In older Emacs version (< 24.4), the method 1 using
   package.el may result in an error something like

   .. sourcecode:: text

      jedi.el:37:1:Error: Cannot open load file: python-environment

   In this case, :kbd:`M-x package-install RET python-environment RET`
   may solve the problem.


.. _make-tryout:

Use ``make tryout`` to try Jedi.el without installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is recommended if you want to develop Jedi.el.  If you have cask_
command, then you just have to:

.. sourcecode:: sh

   git clone https://github.com/tkf/emacs-jedi.git
   cd emacs-jedi
   make tryout

If you are too lazy to go to cask_ site to checkout how to install it,
here is what you need to do::

  curl -fsSkL https://raw.github.com/cask/cask/master/go | python
  make CASK=$HOME/.cask/bin/cask tryout

``make tryout`` will install requirements for Jedi.el separated from
your local setup in ``~/.emacs.d/``.  You can also check the
configuration file tryout-jedi.el_ to see a minimum working
configuration.  This is the configuration file loaded by ``make tryout``.

.. _cask: https://github.com/cask/cask
.. _tryout-jedi.el:
   https://github.com/tkf/emacs-jedi/blob/master/tryout-jedi.el

If you install cask_ in a different place or you don't add it to the
``$PATH``, you can call ``make`` like this: ``make
CASK=PATH/TO/bin/cask tryout``.  Typically, ``PATH/TO/bin/cask`` is
``~/.cask/bin/cask``.


How to update Python dependencies
---------------------------------

Simply run Emacs command ``M-x jedi:install-server``.
See also :el:symbol:`jedi:install-server`.

.. warning:: The following command does not work as of version 0.2.0
   anymore::

     make requirements
     pip install -r PATH/TO/requirements.txt

.. warning:: (For el-get user) ``M-x el-get-update RET jedi RET``
   will *not* update Python dependencies anymore.


How to use Python 3 (or any other specific version of Python)
-------------------------------------------------------------

Using Python 3 as default Python, only in Jedi.el
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set :el:symbol:`jedi:environment-virtualenv` and
:el:symbol:`jedi:environment-virtualenv`, like this::

   (setq jedi:environment-root "jedi")  ; or any other name you like
   (setq jedi:environment-virtualenv
         (append python-environment-virtualenv
                 '("--python" "/PATH/TO/python3")))

Or you can just set, for example, ``virtualenv3`` if it is run by
Python 3::

   (setq jedi:environment-virtualenv
         (list "virtualenv3" "--system-site-packages"))

.. note:: ``--system-site-packages`` is the default option in
   :el:symbol:`python-environment-virtualenv` so you need it if you
   want to the default behavior.  As to why it is the default, see the
   discussion here: `tkf/emacs-python-environment#3
   <https://github.com/tkf/emacs-python-environment/issues/3>`_.

.. note:: In principle, you can modify
   :el:symbol:`python-environment-virtualenv` and do not touch
   :el:symbol:`jedi:environment-virtualenv` *and*
   :el:symbol:`jedi:environment-root`.  However, it changes default
   environment to use Python 3 so make sure all other Python packages
   you need are compatible with Python 3.


Automatically use appropriate Python version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This can be done by making :el:symbol:`jedi:server-command`,
:el:symbol:`jedi:environment-root` and
:el:symbol:`jedi:environment-virtualenv` buffer local using
:el:symbol:`make-local-variable` and set them appropriately.  See
:el:symbol:`jedi:server-command` for more info.  You might need to use
:el:symbol:`python-environment-bin`.


How to setup proxy
------------------

:el:symbol:`jedi:install-server` runs `pip install`_.  So, you may
want to set proxy for it.  In that case, set :envvar:`http_proxy` and
:envvar:`https_proxy` [#]_.

If you want to set it in ``~/.bashrc``, do something like the
following.  Since Emacs [#]_ and other programs read this variable, it
may be the best approach.::

   export http_proxy=http://[user:passwd@]proxy.server:port
   export https_proxy=https://[user:passwd@]proxy.server:port

If you want to set it in Emacs setting, do something like this::

  (setenv "http_proxy" "http://[user:passwd@]proxy.server:port")
  (setenv "https_proxy" "https://[user:passwd@]proxy.server:port")

If you want to setup proxy only for pip, you can use
:envvar:`PIP_PROXY` instead of :envvar:`http_proxy` and
:envvar:`https_proxy`.  This sets default value for `pip --proxy option`_
(see also `pip > User Guide > Environment Variables`_).

.. [#] See also:
   http://stackoverflow.com/questions/14149422/using-pip-behind-a-proxy

   (BTW, :envvar:`http_proxy` and :envvar:`https_proxy` are not
   mentioned in pip manual.  If anybody know official information from
   pip regarding :envvar:`http_proxy` and :envvar:`https_proxy`,
   please add it here.)

.. [#]
   `Proxies - URL Programmer's Manual
   <http://www.gnu.org/software/emacs/manual/html_node/url/Proxies.html>`_

.. _`pip install`:
  http://pip.readthedocs.org/en/latest/reference/pip_install.html

.. _`pip --proxy option`:
  http://pip.readthedocs.org/en/latest/reference/pip.html#cmdoption--proxy

.. _`pip > User Guide > Environment Variables`:
  http://pip.readthedocs.org/en/latest/user_guide.html#environment-variables


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


More resources
==============

.. toctree::

   deprecation
   changelog
