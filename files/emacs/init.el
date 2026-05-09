;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)

(require 'l-lib)

;;; Remote code execution:
;;; https://github.com/califio/publications/blob/main/MADBugs/vim-vs-emacs-vs-claude/Emacs.md
(setopt vc-handled-backends nil)

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
(require 'b-ledger)
(require 'b-gptel)
(require 'b-commands)

(use-package b-org
  :ensure nil
  :commands (b/clock-out-on-screen-lock b/org-goto-last-capture))

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c o b" . b/org-capture-clocked-hypr-ripgrep)
         ("C-c o c" . org-clock-goto)
         ("C-c o g" . b/org-goto-heading)
	 ("C-c o i" . b/org-clock-in-select)
	 ("C-c o l" . b/org-goto-last-capture)
	 ("C-c o o" . b/org-clock-out)
	 ("C-c o p" . b/org-save-and-push-files)
	 ("C-c o r" . b/org-add-note-to-selected)
	 ("C-c o z" . b/org-add-note-to-clocked)
	 ("C-c r" . org-capture))
  :mode (("\\.org\\'" . org-mode))
  :config
  (require 'b-org))

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n r" . org-roam-capture))
  :bind-keymap ("C-c d" . org-roam-dailies-map)
  :commands (org-roam-db-autosync-mode)
  :config
  (require 'b-org)
  (when (file-exists-p "~/org/roam")
    (org-roam-db-autosync-mode)))

(use-package b-ripgrep
  :ensure nil
  :commands (b/ripgrep)
  :bind (("C-x p r" . b/ripgrep-project)))


(defun b/tab-1 () (interactive) (tab-bar-select-tab 1))
(defun b/tab-2 () (interactive) (tab-bar-select-tab 2))
(defun b/tab-3 () (interactive) (tab-bar-select-tab 3))
(defun b/tab-4 () (interactive) (tab-bar-select-tab 4))
(defun b/tab-5 () (interactive) (tab-bar-select-tab 5))
(defun b/tab-6 () (interactive) (tab-bar-select-tab 6))
(defun b/tab-7 () (interactive) (tab-bar-select-tab 7))
(defun b/tab-8 () (interactive) (tab-bar-select-tab 8))
(defun b/tab-9 () (interactive) (tab-bar-select-tab 9))

(use-package tab-bar
  :ensure nil
  :bind (("C-c <left>" . tab-bar-history-back)
         ("C-c <right>" . tab-bar-history-forward)
         ("C-M-s-)" . tab-recent)
         ("C-M-s-!" . b/tab-1)
         ("C-M-s-@" . b/tab-2)
         ("C-M-s-#" . b/tab-3)
         ("C-M-s-$" . b/tab-4)
         ("C-M-s-%" . b/tab-5)
         ("C-M-s-^" . b/tab-6)
         ("C-M-s-&" . b/tab-7)
         ("C-M-s-*" . b/tab-8)
         ("C-M-s-(" . b/tab-9))
  :custom
  ((tab-bar-define-keys nil)
   (tab-bar-tab-hints t))
  :init
  (tab-bar-history-mode t))


(autoload 'b/prolog-config "b-prolog")
(use-package prolog
  :ensure nil
  :mode ("\\.prolog\\'" . prolog-mode)
  :config (b/prolog-config))


(which-key-mode t)
(b/hide-ml-mode 'which-key-mode)

(global-auto-revert-mode t)

(setf remote-file-name-access-timeout 2)

(setf save-interprogram-paste-before-kill t)

;;; C-u C-SPC, but C-SPC can be repeated
(setq set-mark-command-repeat-pop t)

;;; No more `C-x o` after `C-h f` or `C-h v`!!!
(setq help-window-select t)

(use-package repeat
  :ensure nil
  :init
  (setq repeat-exit-timeout 5)
  (repeat-mode 1))

(use-package avy
  :ensure nil
  :bind (("<f23>" . avy-goto-char-timer)))

(use-package hyperbole
  :ensure nil
  :no-require t
  :bind (("<f22>" . hkey-either)))

(keymap-global-set "C-g" #'prot/keyboard-quit-dwim)

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode t))

(require 'b-server)
