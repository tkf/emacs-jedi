==============================================
 Jedi.el - a Python auto-completion for Emacs
==============================================

.. warning:: Work in progress!


Jedi.el is a Python auto-completion package for Emacs.  It uses jedi_
library to compute completion and EPC_ (an RPC stack for Emacs Lisp)
and its `Python binding`_ to commentate with Python process.

.. _jedi: https://github.com/davidhalter/jedi
.. _EPC: https://github.com/kiwanami/emacs-epc
.. _Python binding: python-epc_
.. _python-epc: https://github.com/tkf/python-epc


Install
=======

el-get
------

The easiest way to install Jedi.el is to use el-get_:
just do ``M-x el-get-install jedi``.
You need to have `virtualenv` to automatically install Python module
dependencies.  If your el-get does not have the recipes for Jedi.el
yet, get them from `this pull request`_.

.. _el-get: https://github.com/dimitri/el-get
.. _this pull request: https://github.com/dimitri/el-get/pull/927

Manual install
--------------

1. Install EPC_.
2. Install Jedi.el.  Download this repository and add it to
   `load-path`.
3. Install Jedi_ and python-epc_ by ``make requirements`` or ``pip
   install jedi epc`` if you want to determine where to install them.
   If you don't use the make command, you need to set
   `jedi:server-command` appropriately.
