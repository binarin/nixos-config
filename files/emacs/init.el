;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(setf server-name "emacs-clean")
(server-start)

(setf auto-save-list-file-prefix (file-name-concat b/xdg-runtime-dir))

(setf custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(savehist-mode t)

;; Mostly do not want to track down which sensitive files should be excluded from backups
(setf make-backup-file nil)

(require 'recentf)
(setf recentf-max-saved-items 200
      recentf-auto-cleanup 300)
(setopt recentf-autosave-interval 60)
(recentf-mode t)

(setf inhibit-startup-screen t)
(context-menu-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font-16"))

(winner-mode t)

(require 'which-key)
(which-key-mode t)

(use-package direnv
  :ensure t)
(direnv-mode t)
(setf direnv-show-paths-in-summary nil)

(defun binarin/direnv--summarise-changes (items)
  (list (cl-remove-if-not (lambda (i) (string= (car i) "PATH")) (car items))))

(advice-add 'direnv--summarise-changes :filter-args #'binarin/direnv--summarise-changes)

(setf enable-recursive-minibuffers t)


;; Hide commands in M-x which do not work in the current mode.  Vertico
;; commands are hidden in normal buffers. This setting is useful beyond
;; Vertico.
(setf read-extended-command-predicate #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setf minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(setf remote-file-name-access-timeout 2)

(put 'set-goal-column 'disabled nil)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(fset 'yes-or-no-p 'y-or-n-p)

;;; Completion:
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(setf vertico-multiform-categories
      '((file (:keymap . vertico-directory-map))))

(vertico-multiform-mode)

(keymap-set vertico-directory-map "C-l" #'vertico-directory-delete-word)
;; XXX - not working, picking DEL from global map
;; (keymap-set vertico-directory-map "DEL" #'vertico-directory-delete-char)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))


(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :ensure t
  ;; XXX continue binding
  :bind (([remap switch-to-buffer] . consult-buffer)
	 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
	 ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
	 ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
	 ([remap yank-pop] . consult-yank-pop))
  :config
  (setf xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode t))


(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(defun b/prog-mode-hook ()
  (setq-local dabbrev-case-replace nil
	      dabbrev-case-fold-search nil))

(add-hook 'prog-mode-hook #'b/prog-mode-hook)

(setf tab-always-indent 'complete

      ;; TAB cycle if there are only few candidates
      completion-cycle-threshold 3

      ;; partial-completion is important for file wildcard
      ;; support. Multiple files can be opened at once with find-file if
      ;; you enter a wildcard.
      completion-category-overrides '((file (styles partial-completion)))

      ;; Disable defaults, use our settings
      completion-category-defaults nil
      ;; Emacs 31: partial-completion behaves like substring
      completion-pcm-leading-wildcard t)

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map
	 ("RET" . paredit-newline)
	 ("C-j". nil)))

(setf treesit-font-lock-level 4)

;;; Go

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(defun binarin/go-ts-mode-hook ()
  (eglot-ensure)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
  (electric-pair-local-mode t))

(global-set-key (kbd "<f16>") 'recompile)

(setf compilation-ask-about-save nil)

(add-hook 'go-ts-mode-hook 'binarin/go-ts-mode-hook)

(defun b/yaml-mode-hook ()
  (setf tab-width 2))

(add-hook 'yaml-ts-mode-hook #'b/yaml-mode-hook)

(defvar b/format-buffer-with-error-buffer)
(make-variable-buffer-local 'b/format-buffer-with-error-buffer)

(cl-defun b/format-buffer-with (prog &key display-errors args)
  (let ((stdout-file (make-temp-file "*fmt out"))
	(stderr-file (make-temp-file "*fmt err"))
	stderr-buffer)
    (unwind-protect
      (save-restriction
	(widen)
	(let ((exit-code
	       (apply #'call-process-region nil nil ;; start, end
				    prog
				    nil ;; delete
				    (list (list :file stdout-file) stderr-file) ;; buffer
				    nil ;; display
				    args)))
	  (if (and (numberp exit-code) (eql exit-code 0))
	      (progn
		(insert-file-contents stdout-file nil nil nil t)
		(when (buffer-live-p b/format-buffer-with-error-buffer)
		  (delete-windows-on b/format-buffer-with-error-buffer)
		  (setf b/format-buffer-with-error-buffer nil)))
	    (progn
	      (unless (buffer-live-p b/format-buffer-with-error-buffer)
		(setf b/format-buffer-with-error-buffer
		      (generate-new-buffer (format "%s errors" (buffer-file-name)) t)))
	      (setf stderr-buffer b/format-buffer-with-error-buffer)
	      (with-current-buffer b/format-buffer-with-error-buffer
		(let ((inhibit-read-only t))
		  (insert-file-contents stderr-file nil nil nil t))
		(compilation-mode)
		(if display-errors
		    (display-buffer stderr-buffer)
		  (message "'%s' failed: see '%s'" prog stderr-buffer)))))))
      (delete-file stderr-file)
      (delete-file stdout-file))))

(defun b/format-nix (&optional display-errors)
  (interactive "P")
  (b/format-buffer-with
   "nixfmt"
   :display-errors display-errors
   :args (list "-f" (file-name-nondirectory (buffer-file-name)))))

(defun b/nix-mode-hook ()
  (add-hook 'before-save-hook #'b/format-nix nil t))

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode)
  :hook ((nix-mode . b/nix-mode-hook)))

(use-package cperl-mode
  :ensure nil
  :mode ("\\.\\(pl\\|pm\\)\\'"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit)))
