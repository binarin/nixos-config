;; -*- lexical-binding: t; -*-
(use-package server
  :ensure t)
(setf server-name "emacs-clean")

(when (display-graphic-p)
  (server-start))



(provide 'b-server)
