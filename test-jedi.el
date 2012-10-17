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

(require 'ert)
(require 'jedi)

(defvar jedi:testing-sample-source "
import json
json.l")

(ert-deftest jedi:complete-request ()
  (deferred:sync!
    (with-temp-buffer
      (erase-buffer)
      (insert jedi:testing-sample-source)
      (jedi:complete-request)))
  (should (equal (jedi:ac-direct-matches) '("load" "loads"))))

(provide 'test-jedi)

;;; test-jedi.el ends here
