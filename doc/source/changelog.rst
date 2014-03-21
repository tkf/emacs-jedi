Changelog
=========

v0.2.0 (WIP)
------------

Since there is a backward incompatible change in installation method,
the version is bumped from 0.1.x to 0.2.x (I am not using any
versioning rule, though).

- Python modules are installed via python-environment.el_ now.  No
  need to run ``make requirements`` outside of Emacs.
- In fact, ``make requirements`` is obsolete now.  Running this
  command print some error message for help and exit with an error.
- StackOverflow_ is the main place for questions now.
  Previously it was treated in GitHub `issue tracker`_.
- :el:symbol:`jedi:show-setup-info` is added to help bug reports and
  questions.
- :el:symbol:`jedi:install-python-jedi-dev` is added.

Contributions from:

.. include:: releases/v0.2.0/authors.txt

Closed issues and pulled patches:

   .. include:: releases/v0.2.0/closed.txt

.. _python-environment.el: https://github.com/tkf/emacs-python-environment
.. _StackOverflow: http://stackoverflow.com/questions/tagged/emacs-jedi
.. _issue tracker: https://github.com/tkf/emacs-jedi/issues


v0.1.3 (never released)
-----------------------

(This version is not released.  All changes below are available in
v0.2.0.)

Highlights:

- `Syohei YOSHIDA`_ (@syohex) is in our team.  Welcome!
- Add :el:symbol:`jedi:toggle-log-traceback` and
  :el:symbol:`jedi:pop-to-epc-buffer`.
- Add default keybind and a simple way to setup recommended keybinds.
  See issue `#47`_ for the reason and discussion behind this change.
- Now :el:symbol:`jedi:ac-setup` auto-magically enables auto-complete-mode.
  This is to help people from setting up auto-complete when they were not
  using it before installing jedi.el.  See:
  `#40 <https://github.com/tkf/emacs-jedi/issues/40>`_,
  `bbatsov/prelude#251 <https://github.com/bbatsov/prelude/issues/251>`_,
  `this question in StackOverflow
  <http://stackoverflow.com/questions/15658963>`_.

Contributions from:

.. include:: releases/v0.1.3/authors.txt

Closed issues and pulled patches:

   .. include:: releases/v0.1.3/closed.txt

v0.1.2 (2013-05-26)
-------------------

Highlights:

- Jedi.el is `available from Marmalade
  <http://marmalade-repo.org/packages/jedi>`_.
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

.. _Ting-Yu Lin: https://github.com/aethanyc
.. _abyssly: https://github.com/abyssly
.. _Akihiro Uchida: https://github.com/uchida
.. _Sean Dague: https://github.com/sdague
.. _Thomas Frössman: https://github.com/thomasf
.. _Vyacheslav Levit: https://github.com/vlevit
.. _Aaron Meurer: https://github.com/asmeurer
.. _Danilo Bargen: https://github.com/dbrgn
.. _Fabián Ezequiel Gallina: https://github.com/fgallina
.. _immerrr: https://github.com/immerrr
.. _Jaakko Pallari: https://github.com/jkpl
.. _Kiyono Goro: https://github.com/goro1080
.. _L42y: https://github.com/L42y
.. _Ryan Olf: https://github.com/ryanolf
.. _Syohei YOSHIDA: https://github.com/syohex


.. Links
.. _jedi: https://github.com/davidhalter/jedi
