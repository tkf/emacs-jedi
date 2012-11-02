;;; jedi.el --- a Python auto-completion for Emacs

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Package-Requires: ((epc "0.1.0") (auto-complete "1.4"))

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

(require 'epc)
(require 'auto-complete)
(declare-function pos-tip-show "pos-tip")


(defgroup jedi nil
  "Auto-completion for Python."
  :group 'completion
  :prefix "jedi:")

(defvar jedi:source-dir (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory))

(defvar jedi:epc nil)

(defvar jedi:server-script
  (expand-file-name "jediepcserver.py" jedi:source-dir)
  "Full path to Jedi server script file ``jediepcserver.py``.")

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

To see what other arguments Jedi server can take, execute the
following command::

    python jediepcserver.py --help"
  :group 'jedi)

(defun jedi:start-server ()
  (if jedi:epc
      (message "Jedi server is already started!")
    (let ((default-directory jedi:source-dir))
      (setq jedi:epc (epc:start-epc (car jedi:server-command)
                                    (append (cdr jedi:server-command)
                                            jedi:server-args))))
    (set-process-query-on-exit-flag
     (epc:manager-server-process jedi:epc) nil))
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
  (setq jedi:epc nil))

(defun jedi:get-epc ()
  (or jedi:epc (jedi:start-server)))

(defun jedi:start-dedicated-server (command)
  "Start Jedi server dedicated to this buffer.
This is useful, for exmaple, when you want to use different
`sys.path' for some buffer.  When invoked as an interactive
command, it asks you how to start the Jedi server.  You can edit
the command in minibuffer to specify the way Jedi server run.
See also: `jedi:server-args'."
  (interactive
   (list (split-string-and-unquote
          (read-string "Run Jedi server: "
                       (mapconcat
                        #'identity
                        (append jedi:server-command
                                jedi:server-args)
                        " ")))))
  (if (and (local-variable-p 'jedi:epc) jedi:epc)
      (message "Dedicated Jedi server is already running!")
    (make-local-variable 'jedi:epc)
    (let ((jedi:server-command command)
          (jedi:server-args nil))
      (jedi:start-server))))

(defun jedi:call-deferred (method-name)
  "Call ``Script(...).METHOD-NAME`` and return a deferred object."
  (let ((source      (buffer-substring-no-properties (point-min) (point-max)))
        (line        (count-lines (point-min) (point)))
        (column      (current-column))
        (source-path buffer-file-name))
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
(defun jedi:complete ()
  "Complete code at point."
  (interactive)
  (deferred:nextc (jedi:complete-request)
    (lambda () (auto-complete '(ac-source-jedi-direct)))))

(defcustom jedi:complete-on-dot nil
  "Non-`nil' means automatically start completion after inserting a dot.
To make this option work, you need to use `jedi:setup' instead of
`jedi:ac-setup' to start Jedi."
  :group 'jedi)

(defun jedi:dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (jedi:complete))


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
    (prefix jedi:ac-direct-prefix)
    (init . jedi:complete-request)
    (requires . -1)))

;;;###autoload
(defun jedi:ac-setup ()
  "Add Jedi AC sources to `ac-sources'."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-jedi-direct))


;;; Call signature (get_in_function_call)

(defun* jedi:get-in-function-call--construct-call-signature
    (&key params index call_name)
  (concat call_name "(" (mapconcat #'identity params ", ") ")"))

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

(defvar jedi:get-in-function-call--d nil)

(defcustom jedi:get-in-function-call-timeout 3000
  "Cancel request to server for call signature after this period
specified in in millisecond."
  :group 'jedi)

(defcustom  jedi:get-in-function-call-delay 1000
  "How long Jedi should wait before showing call signature
tooltip in millisecond."
  :group 'jedi)

(defun jedi:get-in-function-call-when-idle ()
  "Show tooltip when Emacs is ilde."
  (unless jedi:get-in-function-call--d
    (setq jedi:get-in-function-call--d
          (deferred:$
            (deferred:wait-idle jedi:get-in-function-call-delay)
            (deferred:nextc it
              (lambda ()
                (deferred:timeout
                  jedi:get-in-function-call-timeout
                  nil
                  (jedi:call-deferred 'get_in_function_call))))
            (deferred:nextc it
              (lambda (reply)
                (jedi:get-in-function-call--tooltip-show reply)
                (setq jedi:get-in-function-call--d nil)))))))

(defcustom jedi:tooltip-method '(pos-tip popup)
  "Configuration for `jedi:tooltip-show'.
This is a list which may contain symbol(s) `pos-tip' and/or
`popup'.  It determines tooltip method to use.  Setting this
value to nil means to use minibuffer instead of tooltip."
  :group 'jedi)

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

(defun jedi:goto-definition (&optional other-window)
  "Goto definition of the object at point."
  (interactive "P")
  (lexical-let ((other-window other-window))
    (deferred:nextc (jedi:call-deferred 'goto)
      (lambda (reply)
        (when reply
          (destructuring-bind (&key line_nr module_path)
              (car reply)
            (funcall (if other-window #'find-file-other-window #'find-file)
                     module_path)
            (goto-char (point-min))
            (forward-line (1- line_nr))))))))


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
         :sources (list (jedi:related-names--source "Jeid Related Names"
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

(defcustom jedi:doc-mode 'rst-mode
  "Major mode to use when showing document."
  :group 'jedi)

(defun jedi:show-doc ()
  "Goto definition of the object at point."
  (interactive)
  (deferred:nextc (jedi:call-deferred 'get_definition)
    (lambda (reply)
      (with-current-buffer (get-buffer-create jedi:doc-buffer-name)
        (erase-buffer)
        (loop with has-doc = nil
              for def in reply
              do (destructuring-bind
                     (&key doc desc_with_module line_nr module_path)
                     def
                   (unless (or (null doc) (equal doc ""))
                     (insert "Docstring for " desc_with_module "\n\n" doc)
                     (setq has-doc t)))
              finally do
              (when has-doc
                (progn
                  (goto-char (point-min))
                  (when (fboundp jedi:doc-mode)
                    (funcall jedi:doc-mode))
                  (pop-to-buffer (current-buffer)))))))))


;;; Meta info

(defun jedi:get-jedi-version-request ()
  "Request version of Python modules and return a deferred object."
  (epc:call-deferred (jedi:get-epc) 'get_jedi_version nil))

(defun jedi:show-jedi-version ()
  (interactive)
  (deferred:nextc (jedi:get-jedi-version-request)
    (lambda (reply)
      (let ((standard-output (get-buffer-create "*jedi:version*")))
        (with-current-buffer standard-output
          (emacs-lisp-mode)
          (erase-buffer)
          (pp reply)
          (display-buffer standard-output))))))


;;; Jedi mode

(defvar jedi-mode-map (make-sparse-keymap))

(defun jedi:handle-post-command ()
  (jedi:get-in-function-call-when-idle))

(define-minor-mode jedi-mode
  "Jedi mode.
When `jedi-mode' is on, call signature is automatically shown as
toolitp when inside of function call."
  :keymap jedi-mode-map
  :group 'jedi
  (let ((map jedi-mode-map))
    (if jedi:complete-on-dot
        (define-key map "." 'jedi:dot-complete)
      (define-key map "." nil)))
  (if jedi-mode
      (add-hook 'post-command-hook 'jedi:handle-post-command nil t)
    (remove-hook 'post-command-hook 'jedi:handle-post-command t)))


;;; Setup

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
  (jedi-mode 1))


(provide 'jedi)

;;; jedi.el ends here
