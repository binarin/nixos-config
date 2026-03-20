;; -*- lexical-binding: t; -*-
(setf server-name "emacs-clean")

(when (display-graphic-p)
  (server-start))



(provide 'b-server)
