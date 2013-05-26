;; Auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;; Jedi
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
