;;; -*- mode: emacs-lisp; lexical-binding: t -*-

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

(use-package b-org
  :ensure nil
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c r" . org-capture))
  :mode (("\\.org\\'" . org-mode))
  :defer 5)

(winner-mode t)
(which-key-mode t)
(global-auto-revert-mode t)

(setf remote-file-name-access-timeout 2)

(use-package treesit :ensure nil
  :config
  (setf treesit-font-lock-level 4))

(require 'b-server)
