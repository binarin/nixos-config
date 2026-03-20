;; -*- lexical-binding: t; -*-

(use-package direnv
  :ensure t)
(direnv-mode t)
(setf direnv-show-paths-in-summary nil)

(defun binarin/direnv--summarise-changes (items)
  (list (cl-remove-if-not (lambda (i) (string= (car i) "PATH")) (car items))))

(advice-add 'direnv--summarise-changes :filter-args #'binarin/direnv--summarise-changes)

(provide 'b-direnv)
