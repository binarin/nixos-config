;; -*- lexical-binding: t; -*-
(require 'prolog)

(defun b/prolog-mode-hook ()
  (setq-local electric-indent-chars
              (cons ?\. electric-indent-chars)))

(defun b/prolog-config ()
  (setf prolog-system 'scryer)
  (setf prolog-program-name "scryer-prolog")
  (setf prolog-indent-width 4)
  (add-hook 'prolog-mode-hook #'b/prolog-mode-hook))

(provide 'b-prolog)
