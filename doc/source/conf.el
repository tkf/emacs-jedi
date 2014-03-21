(defun jedi:parent-dir (path)
  (file-name-directory (directory-file-name path)))

;; Load carton.el
(add-to-list 'load-path
             (jedi:parent-dir (jedi:parent-dir (executable-find "cask"))))
(load "cask")

;; Setup `load-path' using cask.el and package.el
(let* ((doc-source-path (file-name-directory load-file-name))
       (project-path (jedi:parent-dir (jedi:parent-dir doc-source-path))))
  (add-to-list 'load-path project-path)
  (cask-initialize project-path))

;; Load Jedi.el
(require 'jedi)
