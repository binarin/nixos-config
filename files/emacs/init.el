;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(setf inhibit-startup-screen t)

(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font-16"))

(winner-mode t)

(use-package direnv
  :ensure t)
(direnv-mode t)
(setf direnv-show-paths-in-summary nil)

(defun binarin/direnv--summarise-changes (items)
  (list (cl-remove-if-not (lambda (i) (string= (car i) "PATH")) (car items))))

(advice-add 'direnv--summarise-changes :filter-args #'binarin/direnv--summarise-changes)


(require 'which-key)
(which-key-mode t)

(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(savehist-mode t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring


;; (icomplete-vertical-mode t)
;; (setf icomplete-in-buffer t
;;       read-buffer-completion-ignore-case t
;;       read-file-name-completion-ignore-case t)
;; (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
;; (add-to-list 'completion-styles 'flex)

(setf remote-file-name-access-timeout 3)


(setf recentf-max-saved-items 200
      recentf-save-file (expand-file-name "~/.local/state/emacs/recentf.eld")
      recentf-auto-cleanup 300)
(setopt recentf-autosave-interval 60)
(unless (file-exists-p (file-name-directory recentf-save-file)) (make-directory (file-name-directory recentf-save-file)))
(recentf-mode t)

;; (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)

(put 'set-goal-column 'disabled nil)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(defun binarin/go-ts-mode-hook ()
  (eglot-ensure)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
  (electric-pair-local-mode t)
  (local-set-key (kbd "<f16>") 'recompile))

(setf compilation-ask-about-save nil)

(add-hook 'go-ts-mode-hook 'binarin/go-ts-mode-hook)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package magit
  :ensure nil
  :bind (("C-x g" . magit)))
