;; -*- lexical-binding: t; -*-
(require 'server)

(use-package server
  :ensure nil
  :defer 10
  :config
  (when (display-graphic-p) (server-start)))

(require 'l-lib)

(defun b/org-protocol-lazy-load (orig-fun files client &rest args)
  (message "%s" files)
  (if (any (lambda (f)
	     (string-match (rx "org-protocol:/") (car f)))
	   files)
      (progn
	(require 'b-org)
	(apply 'org--protocol-detect-protocol-server orig-fun files client args))
    (apply orig-fun files client args)))

(advice-add 'server-visit-files :around 'b/org-protocol-lazy-load)
(with-eval-after-load 'org-protocol
  (advice-remove 'server-visit-files 'b/org-protocol-lazy-load))

(provide 'b-server)
