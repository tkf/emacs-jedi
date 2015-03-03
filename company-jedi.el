;;; company-jedi.el --- company-mode completion back-end for Python JEDI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2009, 2011-2014  Free Software Foundation, Inc.

;; Author: Boy <boyw165@gmail.com>
;; Package-Requires: ((emacs "24") (company-mode "0.8.11") (epc "0.1.0") (python-environment "0.0.2"))
;; Version: 0.01

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;  This is a company-backend for emacs-jedi. Add this backend to the
;;  `company-backends' and enjoy the power.
;;  e.g.
;;  ;; Basic usage.
;;  (add-to-list 'company-backends 'company-jedi)
;;  ;; Advanced usage.
;;  (add-to-list 'company-backends '(company-jedi company-files))
;;
;;  Check https://github.com/company-mode/company-mode for details.
;;
;;; Change Log:
;;  0.0.1
;;  - Initial release.
;;  - Get candidates through jedi.api.Script.completions().
;;  - 2 customizations to skip completion when point is in a string or comment.
;;
;;; Code:

;; GNU Library.
(require 'cl)
(require 'thingatpt)

(defgroup company-jedi nil
  "Completion back-end for Python JEDI."
  :group 'company)

(defcustom company-jedi-skip-comment-completion t
  "Skip completion prompt when the point is at a comment"
  :type 'boolean
  :group 'company)

(defcustom company-jedi-skip-string-completion t
  "Skip completion prompt when the point is at a string."
  :type 'boolean
  :group 'company)

(defun company-jedi-prefix ()
  (ignore-errors
    (and (eq major-mode 'python-mode)
         (require 'jedi)
         (let ((face (get-text-property (point) 'face))
               (bounds (or (bounds-of-thing-at-point 'symbol)
                           (and (eq (char-before) ?.)
                                (cons (1- (point)) (point)))))
               (thing 'stop))
           (and bounds
                (if (and (eq face 'font-lock-comment-face)
                         company-jedi-skip-comment-completion)
                    nil t)
                (if (and (eq face 'font-lock-string-face)
                         company-jedi-skip-string-completion)
                    nil t)
                (setq thing (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))))
           thing))))

(defun company-jedi-candidates (cb)
  (deferred:nextc
    (jedi:call-deferred 'complete)
    (lambda (reply)
      (let (word
            candidates)
        (dolist (cand reply)
          (and (setq word (plist-get cand :word))
               (push (if (equal company-prefix ".")
                         (concat "." word)
                       word) candidates)))
        (and candidates
             (funcall cb (delete-dups (reverse candidates))))))))

;;;###autoload
(defun company-jedi (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Python JEDI."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-jedi))
    (prefix (company-jedi-prefix))
    (candidates (cons :async 'company-jedi-candidates))
    (meta nil)
    (doc-buffer nil)
    (location nil)))

;;;###autoload
(defun company-jedi--setup ()
  (add-to-list 'company-backends 'company-jedi))

(setq jedi:setup-function #'company-jedi--setup)

(provide 'company-jedi)
;;; company-jedi.el ends here
