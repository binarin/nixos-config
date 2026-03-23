;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)

(require 'l-lib)

(set-language-environment "English")
(set-language-environment-input-method "Russian")

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
(require 'b-windows)
(require 'b-files)
(require 'b-visual)
(require 'b-completion)
(require 'b-direnv)
(require 'b-prog-modes)
(require 'b-compilation)
(require 'b-version-control)

(use-package b-org
  :ensure nil
  :commands (b/clock-out-on-screen-lock b/org-goto-last-capture))

(use-package org
  :ensure nil
  :bind (("C-c o c" . org-clock-goto)
	 ("C-c o l" . b/org-goto-last-capture)
	 ("C-c a" . org-agenda)
	 ("C-c r" . org-capture))
  :mode (("\\.org\\'" . org-mode))
  :config
  (load? 'b-org))

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n r" . org-roam-capture))
  :bind-keymap ("C-c d" . org-roam-dailies-map)
  :commands (org-roam-db-autosync-mode)
  :config
  (load? 'b-org)
  (when (file-exists-p "~/org/roam")
    (org-roam-db-autosync-mode)))

(use-package b-ripgrep
  :ensure nil
  :commands (b/ripgrep)
  :bind (("C-x p r" . b/ripgrep-project)))

(winner-mode t)
(which-key-mode t)
(global-auto-revert-mode t)

(setf remote-file-name-access-timeout 2)

(require 'b-server)
