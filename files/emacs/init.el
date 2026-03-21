;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)

;; lower priority than anything else below
(setf custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'b-startup)
(require 'b-files)
(require 'b-visual)
(require 'b-completion)
(require 'b-direnv)
(require 'b-prog-modes)
(require 'b-compilation)
(require 'b-version-control)

(defun load? (n)
  (unless (featurep n)
    (load (symbol-name n))))

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c r" . org-capture))
  :mode (("\\.org\\'" . org-mode))
  :config
  (load? 'b-org))

(require 'server)
(defun b/org-protocol-lazy-load (orig-fun files client &rest args)
  (message "%s" files)
  (if (any (lambda (f)
	     (string-match (rx "org-protocol:/") (car f)))
	   files)
      (progn
	(load? 'b-org)
	(apply 'org--protocol-detect-protocol-server orig-fun files client args))
    (apply orig-fun files client args)))
(advice-add 'server-visit-files :around 'b/org-protocol-lazy-load)
(with-eval-after-load 'org-protocol
  (advice-remove 'server-visit-files 'b/org-protocol-lazy-load))
(server-start)

(winner-mode t)
(which-key-mode t)
(global-auto-revert-mode t)

(setf remote-file-name-access-timeout 2)
