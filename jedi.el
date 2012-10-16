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

(defvar jedi:source-dir (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory))

(defvar jedi:epc nil)

(defun jedi:start-epc ()
  (if jedi:epc
      (message "Jedi server is already started!")
    (let ((default-directory jedi:source-dir))
      (setq jedi:epc (epc:start-epc "make" '("serve"))))))

(defun jedi:stop-epc ()
  (interactive)
  (if jedi:epc
      (epc:stop-epc jedi:epc)
    (message "Jedi server is already killed."))
  (setq jedi:epc nil))

(defun jedi:get-epc ()
  (or jedi:epc (jedi:start-epc)))


;;; AC source

(defvar jedi:ac-direct-matches nil
  "Variable to store completion candidates for `auto-completion'.")

;; (makunbound 'ac-source-jedi-direct)
(ac-define-source jedi-direct
  '((candidates . jedi:ac-direct-matches)
    (symbol . "s")))

(defun jedi:ac-complete (matches)
  (setq jedi:ac-direct-matches matches)  ; let-binding won't work
  (auto-complete '(ac-source-jedi-direct)))


;;; Completion

(defun jedi:complete (source line column source-path)
  (interactive (list (buffer-substring-no-properties (point-min) (point-max))
                     (count-lines (point-min) (point))
                     (current-column)
                     buffer-file-name))
  (deferred:$
    (epc:call-deferred (jedi:get-epc) 'complete
                       (list source line column source-path))
    (deferred:nextc it
      (lambda (completions)
        (jedi:ac-complete completions)))))


(provide 'jedi)

;;; jedi.el ends here
