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

(require 'mocker)

(require 'jedi)


(defun jedi-testing:sync (d)
  (epc:sync (jedi:get-epc) d))

(ert-deftest jedi:version ()
  "Check if `jedi:version' can be parsed by `version-to-list'."
  (version-to-list jedi:version))


;;; EPC

(ert-deftest jedi:complete-request ()
  (jedi-testing:sync
    (with-temp-buffer
      (erase-buffer)
      (insert "import json" "\n" "json.l")
      (jedi:complete-request)))
  (should (equal (sort (jedi:ac-direct-matches) #'string-lessp)
                 '("load" "loads"))))

(ert-deftest jedi:get-in-function-call-request ()
  (destructuring-bind (&key params index call_name)
      (jedi-testing:sync
        (with-temp-buffer
          (erase-buffer)
          (insert "isinstance(obj,")
          (jedi:call-deferred 'get_in_function_call)))
    (should (equal params '("object" "class_or_type_or_tuple")))
    (should (equal index 1))
    (should (equal call_name "isinstance"))))

(ert-deftest jedi:goto-request ()
  (let ((reply
         (jedi-testing:sync
           (with-temp-buffer
             (erase-buffer)
             (insert "import json" "\n" "json.load")
             (jedi:call-deferred 'goto)))))
    (destructuring-bind (&key line_nr module_path
                              column module_name description)
        (car reply)
      (should (integerp line_nr))
      (should (stringp module_path)))))

(ert-deftest jedi:get-definition-request ()
  (let ((reply
         (jedi-testing:sync
           (with-temp-buffer
             (erase-buffer)
             (insert "import json" "\n" "json.load")
             (jedi:call-deferred 'get_definition)))))
    (destructuring-bind (&key doc desc_with_module line_nr module_path
                              full_name)
        (car reply)
      (should (stringp doc))
      (should (stringp desc_with_module))
      (should (integerp line_nr))
      (should (stringp module_path)))))


;;; Server pool

(defmacro jedi-testing:with-mocked-server (start-epc-records
                                           epc--live-p-records
                                           buffers
                                           &rest body)
  (declare (indent 3))
  `(let ((jedi:server-pool--table (make-hash-table :test 'equal))
         ,@(mapcar
            (lambda (b) `(,b (generate-new-buffer "*jedi test*")))
            buffers))
     (mocker-let
         ((jedi:epc--start-epc (x y) ,start-epc-records)
          (jedi:epc--live-p (x) ,epc--live-p-records)
          (jedi:server-pool--gc-when-idle
           ()
           ((:record-cls 'mocker-stub-record))))
       (macrolet ((check-restart (&rest args)
                                 `(jedi-testing:check-start-server ,@args)))
         (unwind-protect
             (progn ,@body)
           (mapc #'kill-buffer (list ,@buffers)))))))

(defun jedi-testing:check-start-server (buffer command server)
  (with-current-buffer buffer
    (should-not jedi:epc)
    (should (eq (let ((jedi:server-command command)
                      (jedi:server-args nil))
                  (jedi:start-server))
                server))
    (should (eq jedi:epc server))))

(ert-deftest jedi:pool-single-server ()
  "Successive call of `jedi:start-server' with the same setup should
return the same server instance."
  (jedi-testing:with-mocked-server
      ;; Mock `epc:start-epc':
      ((:input '("python" ("jediepcserver.py")) :output 'dummy-server))
      ;; Mock `jedi:epc--live-p':
      ((:input '(dummy-server) :output t))
      ;; Buffers to use:
      (buf1 buf2)
    (check-restart buf1 '("python" "jediepcserver.py") 'dummy-server)
    (check-restart buf2 '("python" "jediepcserver.py") 'dummy-server)))

(ert-deftest jedi:pool-per-buffer-server ()
  "Successive call of `jedi:start-server' with different setups should
return the different server instances."
  (jedi-testing:with-mocked-server
      ;; Mock `epc:start-epc':
      ((:input '("python" ("jediepcserver.py")) :output 'dummy-server-1)
       (:input '("python3" ("jediepcserver.py")) :output 'dummy-server-2))
      ;; Mock `jedi:epc--live-p':
      ()
      ;; Buffers to use:
      (buf1 buf2)
    (check-restart buf1 '("python" "jediepcserver.py") 'dummy-server-1)
    (check-restart buf2 '("python3" "jediepcserver.py") 'dummy-server-2)))

(ert-deftest jedi:pool-restart-per-buffer-server ()
  "When one of the server died, only the died server must be
rebooted; not still living ones."
  (jedi-testing:with-mocked-server
      ;; Mock `epc:start-epc':
      ((:input '("python" ("jediepcserver.py")) :output 'dummy-server-1)
       (:input '("python3" ("jediepcserver.py")) :output 'dummy-server-2)
       (:input '("python" ("jediepcserver.py")) :output 'dummy-server-3))
      ;; Mock `jedi:epc--live-p':
      ((:input '(dummy-server-1) :output t :max-occur 1)
       (:input '(dummy-server-1) :output nil) ; server is stopped
       (:input '(dummy-server-2) :output t)
       (:input '(dummy-server-3) :output t))
      ;; Buffers to use:
      (buf1 buf2 buf3)
    (check-restart buf1 '("python" "jediepcserver.py") 'dummy-server-1)
    (check-restart buf2 '("python3" "jediepcserver.py") 'dummy-server-2)
    (check-restart buf3 '("python" "jediepcserver.py") 'dummy-server-1)
    (mapc (lambda (b) (with-current-buffer b (setq jedi:epc nil)))
          (list buf1 buf2 buf3))
    ;; Now, ``(jedi:epc--live-p dummy-server-1)`` will return nil:
    (check-restart buf1 '("python" "jediepcserver.py") 'dummy-server-3)
    (check-restart buf2 '("python3" "jediepcserver.py") 'dummy-server-2)
    (check-restart buf3 '("python" "jediepcserver.py") 'dummy-server-3)))

(provide 'test-jedi)

;;; test-jedi.el ends here
