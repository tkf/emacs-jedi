;;; jedi.el --- a Python auto-completion for Emacs

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

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

(defvar jedi:server-command
  (list (expand-file-name "env/bin/python" jedi:source-dir)
        "jediepcserver.py"))

(defun jedi:start-epc ()
  (if jedi:epc
      (message "Jedi server is already started!")
    (let ((default-directory jedi:source-dir))
      (setq jedi:epc (epc:start-epc (car jedi:server-command)
                                    (cdr jedi:server-command))))))

(defun jedi:stop-epc ()
  (interactive)
  (if jedi:epc
      (epc:stop-epc jedi:epc)
    (message "Jedi server is already killed."))
  (setq jedi:epc nil))

(defun jedi:get-epc ()
  (or jedi:epc (jedi:start-epc)))


;;; Completion

(defvar jedi:complete-reply nil
  "Last reply to `jedi:complete-request'.")

(defvar jedi:complete-request-point 0
  ;; It is passed to `=', so do not initialize this value by `nil'.
  "The point where `jedi:complete-request' is called.")

;; Make a macro to define EPC request
(defun* jedi:complete-request
    (&optional
     (source      (buffer-substring-no-properties (point-min) (point-max)))
     (line        (count-lines (point-min) (point)))
     (column      (current-column))
     (source-path buffer-file-name)
     (point       (point)))
  "Request ``Script(...).complete`` and return a deferred object.
`jedi:complete-reply' is set to the reply sent from the server."
  (setq jedi:complete-request-point point)
  (deferred:nextc (epc:call-deferred (jedi:get-epc)
                                     'complete
                                     (list source line column source-path))
    (lambda (reply)
      (setq jedi:complete-reply reply))))

;;;###autoload
(defun jedi:complete ()
  "Complete code at point."
  (interactive)
  (deferred:nextc (jedi:complete-request)
    (lambda () (auto-complete '(ac-source-jedi-direct)))))


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

(defun* jedi:get-in-function-call-request
    (&optional
     (source      (buffer-substring-no-properties (point-min) (point-max)))
     (line        (count-lines (point-min) (point)))
     (column      (current-column))
     (source-path buffer-file-name)
     (point       (point)))
  "Request ``Script(...).get_in_function_call`` and return a
deferred object."
  (epc:call-deferred (jedi:get-epc)
                     'get_in_function_call
                     (list source line column source-path)))

(defvar jedi:get-in-function-call--requesting nil)

(defun jedi:get-in-function-call ()
  (interactive)
  (unless jedi:get-in-function-call--requesting
    (setq jedi:get-in-function-call--requesting t)
    (deferred:nextc (jedi:get-in-function-call-request)
      (lambda (reply)
        (when reply
          (destructuring-bind (&key params index call_name)
              reply
            (jedi:tooltip-show
             (concat call_name "("
                     (mapconcat #'identity params ", ") ")"))))
        (setq jedi:get-in-function-call--requesting nil)))))

(defun jedi:tooltip-show (string)
  (cond
   ((and window-system (featurep 'pos-tip))
    (pos-tip-show (jedi:string-fill-paragraph string)
                  'popup-tip-face nil nil 0))
   ((featurep 'popup)
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


;;; Jedi mode

(defun jedi:handle-post-command ()
  (jedi:get-in-function-call))

(define-minor-mode jedi-mode
  "Jedi mode."
  :group 'jedi
  (if jedi-mode
      (add-hook 'post-command-hook 'jedi:handle-post-command nil t)
    (remove-hook 'post-command-hook 'jedi:handle-post-command t)))


;;; Setup

;;;###autoload
(defun jedi:setup ()
  (interactive)
  (jedi:ac-setup)
  (jedi-mode 1))


(provide 'jedi)

;;; jedi.el ends here
