;;; jedi.el --- a Python auto-completion for Emacs

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Package-Requires: ((epc "0.1.0") (auto-complete "1.4"))
;; Version: 0.1.3alpha2

;; This file is NOT part of GNU Emacs.

;; jedi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jedi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with jedi.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ring)

(require 'epc)
(require 'auto-complete)
(declare-function pos-tip-show "pos-tip")


(defgroup jedi nil
  "Auto-completion for Python."
  :group 'completion
  :prefix "jedi:")

(defconst jedi:version "0.1.3alpha2")

(defvar jedi:source-dir (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory))

(defvar jedi:epc nil)
(make-variable-buffer-local 'jedi:epc)

(defvar jedi:server-script
  (convert-standard-filename
   (expand-file-name "jediepcserver.py" jedi:source-dir))
  "Full path to Jedi server script file ``jediepcserver.py``.")


;;; Configuration variables

(defcustom jedi:server-command
  (list (let ((py (expand-file-name "env/bin/python" jedi:source-dir)))
          (if (file-exists-p py) py "python"))
        jedi:server-script)
  "Command used to run Jedi server.

If you setup Jedi requirements using ``make requirements`` command,
`jedi:server-command' should be automatically set to::

    '(\"JEDI:SOURCE-DIR/env/bin/python\"
      \"JEDI:SOURCE-DIR/jediepcserver.py\")

Otherwise, it should be set to::

    '(\"python\" \"JEDI:SOURCE-DIR/jediepcserver.py\")

If you want to use your favorite Python executable, set
`jedi:server-command' using::

    (setq jedi:server-command
          (list \"YOUR-FAVORITE-PYTHON\" jedi:server-script))

If you want to pass some arguments to the Jedi server command,
use `jedi:server-command'."
  :group 'jedi)

(defcustom jedi:server-args nil
  "Command line arguments to be appended to `jedi:server-command'.

If you want to add some special `sys.path' when starting Jedi
server, do something like this::

    (setq jedi:server-args
          '(\"--sys-path\" \"MY/SPECIAL/PATH\"
            \"--sys-path\" \"MY/OTHER/SPECIAL/PATH\"))

If you want to include some virtualenv, do something like this.
Note that actual `VIRTUAL_ENV' is treated automatically.  Also,
you need to start Jedi EPC server with the same python version
that you use for the virtualenv.::

    (setq jedi:server-args
          '(\"--virtual-env\" \"SOME/VIRTUAL_ENV_1\"
            \"--virtual-env\" \"SOME/VIRTUAL_ENV_2\"))

To see what other arguments Jedi server can take, execute the
following command::

    python jediepcserver.py --help


**Advanced usage**

Sometimes you want to configure how Jedi server is started per
buffer.  To do that, you should make this variable buffer local
in `python-mode-hook' and set it to some buffer specific variable,
like this::

  (defun my-jedi-server-setup ()
    (let ((cmds (GET-SOME-PROJECT-SPECIFIC-COMMAND))
          (args (GET-SOME-PROJECT-SPECIFIC-ARGS)))
      (when cmds (set (make-local-variable 'jedi:server-command) cmds))
      (when args (set (make-local-variable 'jedi:server-args) args))))

  (add-hook 'python-mode-hook 'my-jedi-server-setup)

Note that Jedi server run by the same command is pooled.  So,
there is only one Jedi server for the same set of command.  If
you want to check how many EPC servers are running, use the EPC
GUI: M-x `epc:controller'.  You will see a table of EPC connections
for Jedi.el and other EPC applications.

If you want to start a new ad-hoc server for the current buffer,
use the command `jedi:start-dedicated-server'."
  :group 'jedi)

(defcustom jedi:complete-on-dot nil
  "Non-`nil' means automatically start completion after inserting a dot.
To make this option work, you need to use `jedi:setup' instead of
`jedi:ac-setup' to start Jedi."
  :group 'jedi)

(defcustom jedi:tooltip-method '(pos-tip popup)
  "Configuration for `jedi:tooltip-show'.
This is a list which may contain symbol(s) `pos-tip' and/or
`popup'.  It determines tooltip method to use.  Setting this
value to nil means to use minibuffer instead of tooltip."
  :group 'jedi)

(defcustom jedi:get-in-function-call-timeout 3000
  "Cancel request to server for call signature after this period
specified in in millisecond."
  :group 'jedi)

(defcustom  jedi:get-in-function-call-delay 1000
  "How long Jedi should wait before showing call signature
tooltip in millisecond."
  :group 'jedi)

(defcustom jedi:goto-definition-config
  '((nil nil        nil)
    (t   nil        nil)
    (nil definition nil)
    (t   definition nil)
    (nil nil        t  )
    (t   nil        t  )
    (nil definition t  )
    (t   definition t  ))
  "Configure how prefix argument modifies `jedi:goto-definition' behavior.

Each element of the list is arguments (list) passed to
`jedi:goto-definition'.  Note that this variable has no effect on
`jedi:goto-definition' when it is used as a lisp function

The following setting is default (last parts are omitted).
Nth element is used as the argument when N universal prefix
arguments (``C-u``) are given.::

    (setq jedi:goto-definition-config
          '((nil nil        nil)        ; C-.
            (t   nil        nil)        ; C-u C-.
            (nil definition nil)        ; C-u C-u C-.
            (t   definition nil)        ; C-u C-u C-u C-.
            ...))

For example, if you want to follow \"substitution path\" by default,
use the setting like this::

    (setq jedi:goto-definition-config
          '((nil definition nil)
            (t   definition nil)
            (nil nil        nil)
            (t   nil        nil)
            (nil definition t  )
            (t   definition t  )
            (nil nil        t  )
            (t   nil        t  )))

You can rearrange the order to have most useful sets of arguments
at the top."
  :group 'jedi)

(defcustom jedi:doc-mode 'rst-mode
  "Major mode to use when showing document."
  :group 'jedi)

(defcustom jedi:doc-hook '(view-mode)
  "The hook that's run after showing a document."
  :type 'hook
  :group 'jedi)

(defcustom jedi:doc-display-buffer 'display-buffer
  "A function to be called with a buffer to show document."
  :group 'jedi)

(defcustom jedi:install-imenu nil
  "[EXPERIMENTAL] If `t', use Jedi to create `imenu' index.
To use this feature, you need to install the developmental
version (\"dev\" branch) of Jedi."
  :group 'jedi)

(defcustom jedi:imenu-create-index-function 'jedi:create-nested-imenu-index
  "`imenu-create-index-function' for Jedi.el.
It must be a function that takes no argument and return an object
described in `imenu--index-alist'.
This can be set to `jedi:create-flat-imenu-index'.
Default is `jedi:create-nested-imenu-index'."
  :group 'jedi)

(make-obsolete-variable 'jedi:setup-keys nil "0.1.3")
(defcustom jedi:setup-keys nil
  "Setup recommended keybinds.

.. warning:: Use of this value is obsolete now.
   As of 0.1.3, jedi.el has default keybinds.

.. admonition:: Default keybinds

   ``<C-tab>`` : = `jedi:key-complete'
       Complete code at point. (`jedi:complete')

   ``C-.`` : = `jedi:key-goto-definition'
       Goto the definition of the object at point. (`jedi:goto-definition')

   ``C-c d`` : = `jedi:key-show-doc'
       Goto the definition of the object at point. (`jedi:show-doc')

   ``C-c r`` : = `jedi:key-related-names'
       Find related names of the object at point.
       (`helm-jedi-related-names' / `anything-jedi-related-names')

When `jedi:setup-keys' is non-`nil', recommended keybinds are set
in `jedi-mode-map' when **loading** jedi.el.  Therefore, you must
set this value before jedi.el is loaded.  As recommended usage of
jedi.el is to call `jedi:setup' via `python-mode-hook' where
`jedi:setup' is autloaded, setting `jedi:setup-keys' to `t' in
you emacs setup (e.g., ``.emacs.d/init.el``) works fine.::

    (setq jedi:setup-keys t)
    (add-hook 'python-mode-hook 'jedi:setup)

If you want to require jedi.el explicitly when loading Emacs,
make sure to set `jedi:setup-keys' before loading jedi.el::

    (setq jedi:setup-keys t)
    (require 'jedi)

Byte compiler warns about unbound variable if you set
`jedi:setup-keys' before loading jedi.el.  The proper way to
suppress this warning is the following::

    (eval-when-compile (require 'jedi nil t))
    (setq jedi:setup-keys t)

You can change these keybinds by changing `jedi:key-complete',
`jedi:key-goto-definition', `jedi:key-show-doc', and
`jedi:key-related-names'.  For example, default keybind for
ropemacs's `rope-show-doc' is same as `jedi:show-doc'.  You can
avoid collision by something like this::

    (setq jedi:key-show-doc (kbd \"C-c D\"))"
  :group 'jedi)

(defcustom jedi:key-complete (kbd "<C-tab>")
  "Keybind for command `jedi:complete'."
  :group 'jedi)

(defcustom jedi:key-goto-definition (kbd "C-.")
  "Keybind for command `jedi:goto-definition'."
  :group 'jedi)

(defcustom jedi:key-show-doc (kbd "C-c d")
  "Keybind for command `jedi:show-doc'."
  :group 'jedi)

(defcustom jedi:key-related-names (kbd "C-c r")
  "Keybind for command `helm-jedi-related-names' or
`anything-jedi-related-names'."
  :group 'jedi)

(defcustom jedi:key-goto-definition-pop-marker (kbd "C-,")
  "Keybind for command `jedi:goto-definition-pop-marker'."
  :group 'jedi)

(defcustom jedi:use-shortcuts nil
  "If non-`nil', enable the following shortcuts:

| ``M-.``  `jedi:goto-definition'
| ``M-,``  `jedi:goto-definition-pop-marker'
"
  :group 'jedi)

(defcustom jedi:import-python-el-settings t
  "Automatically import setting from python.el variables."
  :group 'jedi)

(defcustom jedi:goto-definition-marker-ring-length 16
  "Length of marker ring to store `jedi:goto-definition' call positions"
  :group 'jedi)


;;; Internal variables

(defvar jedi:get-in-function-call--d nil
  "Bounded to deferred object while requesting get-in-function-call.")

(defvar jedi:defined-names--singleton-d nil
  "Bounded to deferred object while requesting defined_names.")


;;; Jedi mode

(defvar jedi-mode-map (make-sparse-keymap))

(defun jedi:handle-post-command ()
  (jedi:get-in-function-call-when-idle))

(define-minor-mode jedi-mode
  "Jedi mode.
When `jedi-mode' is on, call signature is automatically shown as
toolitp when inside of function call.

\\{jedi-mode-map}"
  :keymap jedi-mode-map
  :group 'jedi
  (let ((map jedi-mode-map))
    (when jedi:use-shortcuts
      (define-key map (kbd "M-.") 'jedi:goto-definition)
      (define-key map (kbd "M-,") 'jedi:goto-definition-pop-marker))
    (if jedi:complete-on-dot
        (define-key map "." 'jedi:dot-complete)
      (define-key map "." nil)))
  (if jedi-mode
      (progn
        (when jedi:install-imenu
          (add-hook 'after-change-functions 'jedi:after-change-handler nil t)
          (jedi:defined-names-deferred)
          (setq imenu-create-index-function jedi:imenu-create-index-function))
        (add-hook 'post-command-hook 'jedi:handle-post-command nil t)
        (add-hook 'kill-buffer-hook 'jedi:server-pool--gc-when-idle nil t))
    (remove-hook 'post-command-hook 'jedi:handle-post-command t)
    (remove-hook 'after-change-functions 'jedi:after-change-handler t)
    (remove-hook 'kill-buffer-hook 'jedi:server-pool--gc-when-idle t)
    (jedi:server-pool--gc-when-idle)))

;; Define keybinds.
;; See: https://github.com/tkf/emacs-jedi/issues/47
(let ((map jedi-mode-map))
  (define-key map (kbd "<C-tab>") 'jedi:complete)
  (define-key map (kbd "C-c ?") 'jedi:show-doc)
  (define-key map (kbd "C-c .") 'jedi:goto-definition)
  (define-key map (kbd "C-c ,") 'jedi:goto-definition-pop-marker)
  (let ((command (cond
                  ((featurep 'helm) 'helm-jedi-related-names)
                  ((featurep 'anything) 'anything-jedi-related-names)
                  ((locate-library "helm") 'helm-jedi-related-names)
                  ((locate-library "anything") 'anything-jedi-related-names))))
    (when command
      (define-key map (kbd "C-c /") command))))

(when jedi:setup-keys
  (let ((map jedi-mode-map))
    (define-key map jedi:key-complete        'jedi:complete)
    (define-key map jedi:key-goto-definition 'jedi:goto-definition)
    (define-key map jedi:key-show-doc        'jedi:show-doc)
    (define-key map jedi:key-goto-definition-pop-marker
      'jedi:goto-definition-pop-marker)
    (let ((command (cond
                    ((featurep 'helm) 'helm-jedi-related-names)
                    ((featurep 'anything) 'anything-jedi-related-names))))
      (when command
        (define-key map jedi:key-related-names command)))))


;;; EPC utils

(defun jedi:epc--live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (epc:connection-process (epc:manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

(defun jedi:epc--start-epc (server-prog server-args)
  "Same as `epc:start-epc', but set query-on-exit flag for
associated processes to nil."
  (let ((mngr (epc:start-epc server-prog server-args)))
    (set-process-query-on-exit-flag (epc:connection-process
                                     (epc:manager-connection mngr))
                                    nil)
    (set-process-query-on-exit-flag (epc:manager-server-process mngr) nil)
    mngr))


;;; Server pool

(defvar jedi:server-pool--table (make-hash-table :test 'equal)
  "A hash table that holds a pool of EPC server instances.")

(defun jedi:server-pool--start (command)
  "Get an EPC server instance from server pool by COMMAND as a
key, or start new one if there is none."
  (let ((cached (gethash command jedi:server-pool--table)))
    (if (and cached (jedi:epc--live-p cached))
        cached
      (let* ((default-directory jedi:source-dir)
             (mngr (jedi:epc--start-epc (car command) (cdr command))))
        (puthash command mngr jedi:server-pool--table)
        (jedi:server-pool--gc-when-idle)
        mngr))))

(defun jedi:-get-servers-in-use ()
  "Return a list of non-nil `jedi:epc' in all buffers."
  (loop with mngr-list
        for buffer in (buffer-list)
        for mngr = (with-current-buffer buffer jedi:epc)
        when (and mngr (not (memq mngr mngr-list)))
        collect mngr into mngr-list
        finally return mngr-list))

(defvar jedi:server-pool--gc-timer nil)

(defun jedi:server-pool--gc ()
  "Stop unused servers."
  (let ((servers-in-use (jedi:-get-servers-in-use)))
    (maphash
     (lambda (key mngr)
       (unless (memq mngr servers-in-use)
         (remhash key jedi:server-pool--table)
         (epc:stop-epc mngr)))
     jedi:server-pool--table))
  ;; Clear timer so that GC is started next time
  ;; `jedi:server-pool--gc-when-idle' is called.
  (setq jedi:server-pool--gc-timer nil))

(defun jedi:server-pool--gc-when-idle ()
  "Run `jedi:server-pool--gc' when idle."
  (unless jedi:server-pool--gc-timer
    (setq jedi:server-pool--gc-timer
          (run-with-idle-timer 10 nil 'jedi:server-pool--gc))))


;;; Server management

(defun jedi:start-server ()
  (if (jedi:epc--live-p jedi:epc)
      (message "Jedi server is already started!")
    (setq jedi:epc (jedi:server-pool--start
                    (append jedi:server-command jedi:server-args))))
  jedi:epc)

(defun jedi:stop-server ()
  "Stop Jedi server.  Use this command when you want to restart
Jedi server (e.g., when you changed `jedi:server-command' or
`jedi:server-args').  Jedi srever will be restarted automatically
later when it is needed."
  (interactive)
  (if jedi:epc
      (epc:stop-epc jedi:epc)
    (message "Jedi server is already killed."))
  (setq jedi:epc nil)
  ;; It could be non-nil due to some error.  Rescue it in that case.
  (setq jedi:get-in-function-call--d nil)
  (setq jedi:defined-names--singleton-d nil))

(defun jedi:get-epc ()
  (if (jedi:epc--live-p jedi:epc)
      jedi:epc
    (jedi:start-server)))

;;;###autoload
(defun jedi:start-dedicated-server (command)
  "Start Jedi server dedicated to this buffer.
This is useful, for example, when you want to use different
`sys.path' for some buffer.  When invoked as an interactive
command, it asks you how to start the Jedi server.  You can edit
the command in minibuffer to specify the way Jedi server run.

If you want to setup how Jedi server is started programmatically
per-buffer/per-project basis, make `jedi:server-command' and
`jedi:server-args' buffer local and set it in `python-mode-hook'.
See also: `jedi:server-args'."
  (interactive
   (list (split-string-and-unquote
          (read-string "Run Jedi server: "
                       (mapconcat
                        #'identity
                        (append jedi:server-command
                                jedi:server-args)
                        " ")))))
  ;; Reset `jedi:epc' so that a new server is created when COMMAND is
  ;; new.  If it is already in the server pool, the server instance
  ;; already in the pool is picked up by `jedi:start-server'.
  (setq jedi:epc nil)
  ;; Set `jedi:server-command', so that this command is used
  ;; when restarting EPC server of this buffer.
  (set (make-local-variable 'jedi:server-command) command)
  (set (make-local-variable 'jedi:server-args) nil)
  (jedi:start-server))

(defun jedi:-buffer-file-name ()
  "Return `buffer-file-name' without text properties.
See: https://github.com/tkf/emacs-jedi/issues/54"
  (when (stringp buffer-file-name)
    (substring-no-properties buffer-file-name)))

(defun jedi:call-deferred (method-name)
  "Call ``Script(...).METHOD-NAME`` and return a deferred object."
  (let ((source      (buffer-substring-no-properties (point-min) (point-max)))
        (line        (count-lines (point-min) (min (1+ (point)) (point-max))))
        (column      (current-column))
        (source-path (jedi:-buffer-file-name)))
    (epc:call-deferred (jedi:get-epc)
                       method-name
                       (list source line column source-path))))


;;; Completion

(defvar jedi:complete-reply nil
  "Last reply to `jedi:complete-request'.")

(defvar jedi:complete-request-point 0
  ;; It is passed to `=', so do not initialize this value by `nil'.
  "The point where `jedi:complete-request' is called.")

(defun jedi:complete-request ()
  "Request ``Script(...).complete`` and return a deferred object.
`jedi:complete-reply' is set to the reply sent from the server."
  (setq jedi:complete-request-point (point))
  (deferred:nextc (jedi:call-deferred 'complete)
    (lambda (reply)
      (setq jedi:complete-reply reply))))

;;;###autoload
(defun* jedi:complete (&key (expand ac-expand-on-auto-complete))
  "Complete code at point."
  (interactive)
  (lexical-let ((expand expand))
    (deferred:nextc (jedi:complete-request)
      (lambda ()
        (let ((ac-expand-on-auto-complete expand))
          (ac-start))))))
;; Calling `auto-complete' or `ac-update-greedy' instead of `ac-start'
;; here did not work.

(defun jedi:dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (unless (ac-cursor-on-diable-face-p)
    (jedi:complete :expand nil)))


;;; AC source

(defun jedi:ac-direct-matches ()
  (mapcar
   (lambda (x)
     (destructuring-bind (&key word doc description symbol)
         x
       (popup-make-item word
                        :symbol symbol
                        :document (unless (equal doc "") doc)
                        :summary description)))
   jedi:complete-reply))

(defun jedi:ac-direct-prefix ()
  (or (ac-prefix-default)
      (when (= jedi:complete-request-point (point))
        jedi:complete-request-point)))

;; (makunbound 'ac-source-jedi-direct)
(ac-define-source jedi-direct
  '((candidates . jedi:ac-direct-matches)
    (prefix . jedi:ac-direct-prefix)
    (init . jedi:complete-request)
    (requires . -1)))

;;;###autoload
(defun jedi:ac-setup ()
  "Add Jedi AC sources to `ac-sources'.

If auto-completion is all you need, you can call this function instead
of `jedi:setup', like this::

   (add-hook 'python-mode-hook 'jedi:ac-setup)

Note that this function calls `auto-complete-mode' if it is not
already enabled, for people who don't call `global-auto-complete-mode'
in their Emacs configuration."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  (unless auto-complete-mode
    (auto-complete-mode)))


;;; Call signature (get_in_function_call)

(defface jedi:highlight-function-argument
  '((t (:inherit bold)))
  "Face used for the argument at point in a function's argument list"
  :group 'jedi)

(defun* jedi:get-in-function-call--construct-call-signature
    (&key params index call_name)
  (let ((current-arg (nth index params)))
    (when (and current-arg (null jedi:tooltip-method))
      (setf (nth index params)
            (propertize current-arg 'face 'jedi:highlight-function-argument)))
    (concat call_name "(" (mapconcat #'identity params ", ") ")")))

(defun jedi:get-in-function-call--tooltip-show (args)
  (when (and args (not ac-completing))
    (jedi:tooltip-show
     (apply #'jedi:get-in-function-call--construct-call-signature args))))

(defun jedi:get-in-function-call ()
  "Manually show call signature tooltip."
  (interactive)
  (deferred:nextc
    (jedi:call-deferred 'get_in_function_call)
    #'jedi:get-in-function-call--tooltip-show))

(defun jedi:get-in-function-call-when-idle ()
  "Show tooltip when Emacs is ilde."
  (unless jedi:get-in-function-call--d
    (setq jedi:get-in-function-call--d
          (deferred:try
            (deferred:$
              (deferred:wait-idle jedi:get-in-function-call-delay)
              (deferred:nextc it
                (lambda ()
                  (when jedi-mode         ; cursor may be moved
                    (deferred:timeout
                      jedi:get-in-function-call-timeout
                      nil
                      (jedi:call-deferred 'get_in_function_call)))))
              (deferred:nextc it #'jedi:get-in-function-call--tooltip-show))
            :finally
            (lambda ()
              (setq jedi:get-in-function-call--d nil))))))

(defun jedi:tooltip-show (string)
  (cond
   ((and (memq 'pos-tip jedi:tooltip-method) window-system
         (featurep 'pos-tip))
    (pos-tip-show (jedi:string-fill-paragraph string)
                  'popup-tip-face nil nil 0))
   ((and (memq 'popup jedi:tooltip-method)
         (featurep 'popup))
    (popup-tip string))
   (t (when (stringp string)
        (let ((message-log-max nil))
          (message string))))))

(defun jedi:string-fill-paragraph (string &optional justify)
  (with-temp-buffer
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (fill-paragraph justify)
    (buffer-string)))


;;; Goto

(defvar jedi:goto-definition--index nil)
(defvar jedi:goto-definition--cache nil)
(defvar jedi:goto-definition--marker-ring
  (make-ring jedi:goto-definition-marker-ring-length)
  "Marker ring that stores `jedi:goto-definition' call positions")

(defun jedi:goto-definition (&optional other-window deftype use-cache index)
  "Goto the definition of the object at point.

See `jedi:goto-definition-config' for how this function works
when universal prefix arguments \(``C-u``) are given.  If
*numeric* prefix argument(s) \(e.g., ``M-0``) are given, goto
point of the INDEX-th result.  Note that you cannot mix universal
and numeric prefixes.  It is Emacs's limitation.  If you mix both
kinds of prefix, you get numeric prefix.

When used as a lisp function, popup a buffer when OTHER-WINDOW is
non-nil.  DEFTYPE must be either `assignment' (default) or
`definition'.  When USE-CACHE is non-nil, use the locations of
the last invocation of this command.  If INDEX is specified, goto
INDEX-th result."
  (interactive
   (if (integerp current-prefix-arg)
       (list nil nil nil current-prefix-arg)
     (nth (let ((i (car current-prefix-arg)))
            (if i (floor (log i 4)) 0))
          jedi:goto-definition-config)))
  (cond
   ((and (or use-cache index)
         jedi:goto-definition--cache)
    (setq jedi:goto-definition--index (or index 0))
    (jedi:goto-definition--nth other-window))
   ((and (eq last-command 'jedi:goto-definition)
         (> (length jedi:goto-definition--cache) 1))
    (jedi:goto-definition-next other-window))
   (t
    (setq jedi:goto-definition--index (or index 0))
    (lexical-let ((other-window other-window))
      (deferred:nextc (jedi:call-deferred
                       (case deftype
                         ((assignment nil) 'goto)
                         (definition 'get_definition)
                         (t (error "Unsupported deftype: %s" deftype))))
        (lambda (reply)
          (jedi:goto-definition--callback reply other-window)))))))

(defun jedi:goto-definition-push-marker ()
  "Push point onto goto-definition marker ring."
  (ring-insert jedi:goto-definition--marker-ring (point-marker)))

(defun jedi:goto-definition-pop-marker ()
  "Goto the last point where `jedi:goto-definition' was called."
  (interactive)
  (if (ring-empty-p jedi:goto-definition--marker-ring)
      (error "Jedi marker ring is empty, can't pop")
    (let ((marker (ring-remove jedi:goto-definition--marker-ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "Buffer has been deleted")))
      (goto-char (marker-position marker))
      ;; Cleanup the marker so as to avoid them piling up.
      (set-marker marker nil nil))))

(defun jedi:goto-definition-next (&optional other-window)
  "Goto the next cached definition.  See: `jedi:goto-definition'."
  (interactive "P")
  (let ((len (length jedi:goto-definition--cache))
        (n (1+ jedi:goto-definition--index)))
    (setq jedi:goto-definition--index (if (>= n len) 0 n))
    (jedi:goto-definition--nth other-window)))

(defun jedi:goto-definition--callback (reply other-window)
  (if (not reply)
      (message "Definition not found.")
    (setq jedi:goto-definition--cache reply)
    (jedi:goto-definition--nth other-window t)))

(defun jedi:goto--line-column (line column)
  "Like `goto-char' but specify the position by LINE and COLUMN."
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column))

(defun jedi:goto-definition--nth (other-window &optional try-next)
  (let* ((len (length jedi:goto-definition--cache))
         (n jedi:goto-definition--index)
         (next (lambda ()
                 (when (< n (1- len))
                   (incf jedi:goto-definition--index)
                   (jedi:goto-definition--nth other-window)
                   t))))
    (destructuring-bind (&key line_nr column module_path module_name
                              &allow-other-keys)
        (nth n jedi:goto-definition--cache)
      (cond
       ((equal module_name "__builtin__")
        (unless (and try-next (funcall next))
          (message "Cannot see the definition of __builtin__.")))
       ((not (and module_path (file-exists-p module_path)))
        (unless (and try-next (funcall next))
          (message "File '%s' does not exist." module_path)))
       (t
        (jedi:goto-definition-push-marker)
        (funcall (if other-window #'find-file-other-window #'find-file)
                 module_path)
        (jedi:goto--line-column line_nr column)
        (jedi:goto-definition--notify-alternatives len n))))))

(defun jedi:goto-definition--notify-alternatives (len n)
  (unless (= len 1)
    (message
     "%d-th point in %d candidates.%s"
     (1+ n)
     len
     ;; Note: It must be `last-command', not `last-command' because
     ;;       this function is called in deferred at the first time.
     (if (eq last-command 'jedi:goto-definition)
         (format "  Type %s to go to the next point."
                 (key-description
                  (car (where-is-internal 'jedi:goto-definition))))
       ""))))


;;; Full name

(defun jedi:get-full-name-deferred ()
  (deferred:$
    (jedi:call-deferred 'get_definition)
    (deferred:nextc it
      (lambda (reply)
        (loop for def in reply
              do (destructuring-bind (&key full_name &allow-other-keys)
                     def
                   (when full_name
                     (return full_name))))))))

(defun* jedi:get-full-name-sync (&key (timeout 500))
  (epc:sync
   (jedi:get-epc)
   (deferred:timeout timeout nil (jedi:get-full-name-deferred))))


;;; Related names

(defun jedi:related-names--source (name candidates)
  `((name . ,name)
    (candidates . ,candidates)
    (recenter)
    (type . file-line)))

(defun jedi:related-names--to-file-line (reply)
  (mapcar
   (lambda (x)
     (destructuring-bind
         (&key line_nr column module_name module_path description)
         x
       (format "%s:%s: %s - %s" module_path line_nr
               module_name description)))
   reply))

(defun jedi:related-names--helm (helm)
  (lexical-let ((helm helm))
    (deferred:nextc
      (let ((to-file-line #'jedi:related-names--to-file-line))
        (deferred:parallel
          (deferred:nextc (jedi:call-deferred 'related_names) to-file-line)
          (deferred:nextc (jedi:call-deferred 'goto)          to-file-line)))
      (lambda (candidates-list)
        (funcall
         helm
         :sources (list (jedi:related-names--source "Jedi Related Names"
                                                    (car candidates-list))
                        (jedi:related-names--source "Jedi Goto"
                                                    (cadr candidates-list)))
         :buffer (format "*%s jedi:related-names*" helm))))))

;;;###autoload
(defun helm-jedi-related-names ()
  "Find related names of the object at point using `helm' interface."
  (interactive)
  (jedi:related-names--helm 'helm))

;;;###autoload
(defun anything-jedi-related-names ()
  "Find related names of the object at point using `anything' interface."
  (interactive)
  (jedi:related-names--helm 'anything))


;;; Show document (get-definition)

(defvar jedi:doc-buffer-name "*jedi:doc*")

(defun jedi:show-doc ()
  "Show the documentation of the object at point."
  (interactive)
  (deferred:nextc (jedi:call-deferred 'get_definition)
    (lambda (reply)
      (with-current-buffer (get-buffer-create jedi:doc-buffer-name)
        (loop with has-doc = nil
              with first = t
              with inhibit-read-only = t
              initially (erase-buffer)
              for def in reply
              do (destructuring-bind
                     (&key doc desc_with_module line_nr module_path
                           &allow-other-keys)
                     def
                   (unless (or (null doc) (equal doc ""))
                     (if first
                         (setq first nil)
                       (insert "\n\n---\n\n"))
                     (insert "Docstring for " desc_with_module "\n\n" doc)
                     (setq has-doc t)))
              finally do
              (if (not has-doc)
                  (message "Document not found.")
                (progn
                  (goto-char (point-min))
                  (when (fboundp jedi:doc-mode)
                    (funcall jedi:doc-mode))
                  (run-hooks 'jedi:doc-hook)
                  (funcall jedi:doc-display-buffer (current-buffer)))))))))


;;; Defined names (imenu)

(defvar jedi:defined-names--cache nil)
(make-variable-buffer-local 'jedi:defined-names--cache)

(defun jedi:defined-names-deferred ()
  (deferred:nextc
    (epc:call-deferred
     (jedi:get-epc)
     'defined_names
     (list (buffer-substring-no-properties (point-min) (point-max))
           (jedi:-buffer-file-name)))
    (lambda (reply)
      (setq jedi:defined-names--cache reply))))

(defun jedi:defined-names--singleton-deferred ()
  "Like `jedi:defined-names-deferred', but make sure that only
one request at the time is emitted."
  (unless jedi:defined-names--singleton-d
    (setq jedi:defined-names--singleton-d
          (deferred:watch (jedi:defined-names-deferred)
            (lambda (_) (setq jedi:defined-names--singleton-d nil))))))

(defun jedi:defined-names--sync ()
  (unless jedi:defined-names--cache
    (epc:sync (jedi:get-epc) (jedi:defined-names--singleton-deferred)))
  jedi:defined-names--cache)

(defun jedi:after-change-handler (&rest _)
  (unless (or (ac-menu-live-p) (ac-inline-live-p))
    (jedi:defined-names--singleton-deferred)))

(defun jedi:imenu-make-marker (def)
  (destructuring-bind (&key line_nr column &allow-other-keys) def
    (save-excursion (jedi:goto--line-column line_nr column)
                    (point-marker))))

(defun jedi:create-nested-imenu-index--item (def)
  (cons (plist-get def :name) (jedi:imenu-make-marker def)))

(defun jedi:create-nested-imenu-index ()
  "`imenu-create-index-function' for Jedi.el.
See also `jedi:imenu-create-index-function'."
  (when (called-interactively-p 'interactive) (jedi:defined-names--sync))
  (jedi:create-nested-imenu-index-1))

(defun jedi:create-nested-imenu-index-1 (&optional items)
  (loop for (def . subdefs) in (or items jedi:defined-names--cache)
        if subdefs
        collect (append
                 (list (plist-get def :local_name)
                       (jedi:create-nested-imenu-index--item def))
                 (jedi:create-nested-imenu-index-1 subdefs))
        else
        collect (jedi:create-nested-imenu-index--item def)))

(defun jedi:create-flat-imenu-index ()
  "`imenu-create-index-function' for Jedi.el to create flatten index.
See also `jedi:imenu-create-index-function'."
  (when (called-interactively-p 'interactive) (jedi:defined-names--sync))
  (jedi:create-flat-imenu-index-1))

(defun jedi:create-flat-imenu-index-1 (&optional items)
  (loop for (def . subdefs) in (or items jedi:defined-names--cache)
        collect (cons (plist-get def :local_name) (jedi:imenu-make-marker def))
        when subdefs
        append (jedi:create-flat-imenu-index-1 subdefs)))


;;; Meta info

(defun jedi:get-jedi-version-request ()
  "Request version of Python modules and return a deferred object."
  (epc:call-deferred (jedi:get-epc) 'get_jedi_version nil))

(defun jedi:show-version-info ()
  "Show version info of Python modules used by the server.
Paste the result of this function in bug report."
  (interactive)
  (deferred:nextc (jedi:get-jedi-version-request)
    (lambda (reply)
      (let ((standard-output (get-buffer-create "*jedi:version*")))
        (with-current-buffer standard-output
          (emacs-lisp-mode)
          (erase-buffer)
          (pp `(:emacs-version ,emacs-version :jedi-version ,jedi:version))
          (pp reply)
          (display-buffer standard-output))))))

(define-obsolete-function-alias
  'jedi:show-jedi-version 'jedi:show-version-info "0.1.3")

(defun jedi:print-jedi-version ()
  (pp (epc:sync (jedi:get-epc) (jedi:get-jedi-version-request))))


;;; Setup

(defun jedi:import-python-el-settings-setup ()
  "Make jedi aware of python.el virtualenv and path settings.
This is automatically added to the `jedi-mode-hook' when
`jedi:import-python-el-settings' is non-nil."
  (let ((args))
    (when (bound-and-true-p python-shell-extra-pythonpaths)
      (mapc
       (lambda (path)
         (setq args (append (list "--sys-path" path) args)))
       python-shell-extra-pythonpaths))
    (when (bound-and-true-p python-shell-virtualenv-path)
      (setq args
            (append
             (list "--virtual-env" python-shell-virtualenv-path)
             args)))
    (when args
      (set (make-local-variable 'jedi:server-args)
           (append args jedi:server-args)))))

;;;###autoload
(defun jedi:setup ()
  "Fully setup jedi.el for current buffer.
It setups `ac-sources' (calls `jedi:ac-setup') and turns
`jedi-mode' on.

This function is intended to be called from `python-mode-hook',
like this::

       (add-hook 'python-mode-hook 'jedi:setup)

You can also call this function as a command, to quickly test
what jedi can do."
  (interactive)
  (jedi:ac-setup)
  (when jedi:import-python-el-settings
    ;; Hack to access buffer/dir-local vars: http://bit.ly/Y5IfMV.
    ;; Given that `jedi:setup' is added to the `python-mode-hook'
    ;; this will modify `hack-local-variables-hook' on python
    ;; buffers only and will allow us to access buffer/directory
    ;; local variables in `jedi:import-python-el-settings-setup'.
    (add-hook 'hack-local-variables-hook
              #'jedi:import-python-el-settings-setup nil t))
  (jedi-mode 1))


;;; Debugging

(defun jedi:pop-to-epc-buffer ()
  "Open the buffer associated with EPC server process.
Use this command to see the output (e.g., traceback) of the server process."
  (interactive)
  (pop-to-buffer (process-buffer (epc:manager-server-process jedi:epc))))

(defun jedi:toggle-log-traceback ()
  "Toggle on/off traceback logging for EPC server for the current buffer.
When there is an error during traceback logging is enabled, traceback
is printed in the EPC buffer.  You can use `jedi:pop-to-epc-buffer' to
open that buffer.

You can also pass ``--log-traceback`` option to jediepcserver.py
to start server with traceback logging turned on.  This is useful when
there is a problem in communication (thus this command does not work).
You can use `jedi:start-dedicated-server' to restart EPC server for the
current buffer with specific arguments."
  (interactive)
  (deferred:$
    (epc:call-deferred (jedi:get-epc) 'toggle_log_traceback nil)
    (deferred:nextc it
      (lambda (flag)
        (message "Traceback logging is %s" (if flag "enabled" "disabled"))))))

(defvar jedi:server-command--backup nil)
(defvar jedi:server-args--backup nil)

(defun jedi:toggle-debug-server ()
  "Setup `jedi:server-command' and `jedi:server-args' to debug
server using pdb or ipdb.

When this command is called, it essentially execute the following
code::

  (jedi:stop-server)
  (setq jedi:server-command (list \"cat\" \"jedi-port.log\" )
        jedi:server-args nil)

It means to pass the port number recorded in the file
jedi-port.log to EPC client.

To start Jedi server in terminal and record port to the file,
use the following command::

   python jediepcserver.py --port-file jedi-port.log --pdb

This command will be copied in the kill-ring (clipboard) when
this command is called.  You can use `--ipdb` instead of `--pdb`
to use ipdb instead of pdb.

Calling this command again restores the original setting of
`jedi:server-command' and `jedi:server-args' then stops the
running server."
  (interactive)
  (if jedi:server-command--backup
      (progn
        (setq jedi:server-command jedi:server-command--backup
              jedi:server-command--backup nil
              jedi:server-args jedi:server-args--backup)
        (jedi:stop-server)
        (message "Quit debugging.  Original setting restored."))
    (setq jedi:server-command--backup jedi:server-command
          jedi:server-args--backup jedi:server-args
          jedi:server-command (list "cat" (expand-file-name
                                           "jedi-port.log" jedi:source-dir))
          jedi:server-args nil)
    (jedi:stop-server)
    (kill-new "python jediepcserver.py --port-file jedi-port.log --ipdb")
    (message "Now, start server with: --port-file jedi-port.log --ipdb.\
 (command is copied in the kill-ring)")))


(provide 'jedi)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; jedi.el ends here
