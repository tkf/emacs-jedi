;;; test-jedi.el --- Tests for jedi.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-jedi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; test-jedi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-jedi.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ert)
(require 'jedi)


(ert-deftest jedi:complete-request ()
  (deferred:sync!
    (with-temp-buffer
      (erase-buffer)
      (insert "import json" "\n" "json.l")
      (jedi:complete-request)))
  (should (equal (sort (jedi:ac-direct-matches) #'string-lessp)
                 '("load" "loads"))))

(ert-deftest jedi:get-in-function-call-request ()
  (destructuring-bind (&key params index call_name)
      (deferred:sync!
        (with-temp-buffer
          (erase-buffer)
          (insert "isinstance(obj,")
          (jedi:get-in-function-call-request)))
    (should (equal params '("object" "class_or_type_or_tuple")))
    (should (equal index 1))
    (should (equal call_name "isinstance"))))

(ert-deftest jedi:goto-request ()
  (let ((reply
         (deferred:sync!
           (with-temp-buffer
             (erase-buffer)
             (insert "import json" "\n" "json.load")
             (jedi:goto-request)))))
    (destructuring-bind (&key line_nr module_path)
        (car reply)
      (should (integerp line_nr))
      (should (stringp module_path)))))

(ert-deftest jedi:get-definition-request ()
  (let ((reply
         (deferred:sync!
           (with-temp-buffer
             (erase-buffer)
             (insert "import json" "\n" "json.load")
             (jedi:get-definition-request)))))
    (destructuring-bind (&key doc desc_with_module line_nr module_path)
        (car reply)
      (should (stringp doc))
      (should (stringp desc_with_module))
      (should (integerp line_nr))
      (should (stringp module_path)))))

(provide 'test-jedi)

;;; test-jedi.el ends here
