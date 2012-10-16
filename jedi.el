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
  (if jedi:epc
      (epc:stop-epc jedi:epc)
    (message "Jedi server is already killed."))
  (setq jedi:epc nil))
;; (jedi:stop-epc)

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
