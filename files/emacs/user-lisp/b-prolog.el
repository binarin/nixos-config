;; -*- lexical-binding: t; -*-
(use-package prolog
  :ensure nil
  :mode ("\\.\\(pro\\|prolog\\)\\'")
  :config
  (setq-default prolog-system 'swi
                prolog-program-switches '((swi (eval (list "-O" "-f" (file-name-concat (project-root (project-current)) "init.pl"))))))
  (setf
        prolog-electric-if-then-else-flag t
        prolog-electric-dot-flag t
        prolog-electric-dot-full-predicate-template t ;; not working?
        prolog-electric-underscore-flag nil
        prolog-electric-colon-flag nil))

;; (defun b/prolog-config ()
;;   (setf prolog-system 'scryer)
;;   (setf prolog-program-name "scryer-prolog")
;;   (setf prolog-indent-width 4)
;;   (add-hook 'prolog-mode-hook #'b/prolog-mode-hook))

;; (defun b/prolog-mode-hook ()
;;   (setq-local electric-indent-chars
;;               (cons ?\. electric-indent-chars)))



(provide 'b-prolog)
