;;; jedi.el --- a Python auto-completion for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Package-Requires: ((emacs "24") (jedi-core "0.2.2") (auto-complete "1.4"))
;; Version: 0.2.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'auto-complete)
(require 'jedi-core)

;;; AC source
(defun jedi:ac-direct-matches ()
  (mapcar
   (lambda (x)
     (cl-destructuring-bind (&key word doc description symbol)
         x
       (popup-make-item word
                        :symbol symbol
                        :document (unless (equal doc "") doc)
                        :summary description)))
   jedi:complete-reply))

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

(defun jedi:ac-direct-prefix ()
  (or (ac-prefix-default)
      (when (= jedi:complete-request-point (point))
        jedi:complete-request-point)))

(defun jedi:after-change-handler (&rest _)
  (unless (or (ac-menu-live-p) (ac-inline-live-p))
    (jedi:defined-names--singleton-deferred)))

;; (makunbound 'ac-source-jedi-direct)
(ac-define-source jedi-direct
  '((candidates . jedi:ac-direct-matches)
    (prefix . jedi:ac-direct-prefix)
    (init . jedi:complete-request)
    (requires . -1)))

;;;###autoload
(cl-defun jedi:complete (&key (expand ac-expand-on-auto-complete))
  "Complete code at point."
  (interactive)
  (deferred:nextc (jedi:complete-request)
    (lambda ()
      (let ((ac-expand-on-auto-complete expand))
        (ac-start :triggered 'command)))))
;; Calling `auto-complete' or `ac-update-greedy' instead of `ac-start'
;; here did not work.

(defun jedi:dot-complete (nchars)
  "Insert dot and complete code at point."
  (interactive "p")
  (self-insert-command nchars)
  (unless (or (/= nchars 1) ;; don't complete if inserted 2+ dots
              (ac-cursor-on-diable-face-p)
              ;; don't complete if the dot is immediately after int literal
              (looking-back "\\(\\`\\|[^._[:alnum:]]\\)[0-9]+\\."))
    (jedi:complete :expand nil)))

;;;###autoload
(defun jedi:auto-complete-mode ()
  (let ((map jedi-mode-map))
    (if jedi:complete-on-dot
        (define-key map "." 'jedi:dot-complete)
      (define-key map "." nil)))
  (when jedi:install-imenu
    (if jedi-mode
        (add-hook 'after-change-functions 'jedi:after-change-handler nil t)
      (remove-hook 'after-change-functions 'jedi:after-change-handler t))))

;;;###autoload
(setq jedi:setup-function #'jedi:ac-setup
      jedi:mode-function #'jedi:auto-complete-mode)

(provide 'jedi)
;;; jedi.el ends here
