;; -*- lexical-binding: t; -*-
(require 'b-format-on-save)
(require 'l-lib)
(require 'ws-butler)

(setq-default indent-tabs-mode nil)
(ws-butler-global-mode)

(use-package cperl-mode
  :ensure nil
  :mode ("\\.\\(pl\\|pm\\)\\'"))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("RET" . paredit-newline)
	      ("C-j". nil))
  :config
  (b/hide-ml-mode 'paredit-mode))
 


;;; emacs-lisp
(add-to-list 'trusted-content "~/personal-workspace/nixos-config/files/emacs")
(setf elisp-fontify-semantically t)


(autoload 'b/rainbow-mode "b-rainbow")
(defun b/emacs-lisp-mode-hook ()
  (when (and (buffer-file-name)
	     (equal "zenburn-theme.el" (file-name-nondirectory (buffer-file-name))))
    (b/rainbow-mode t)))
(add-hook 'emacs-lisp-mode-hook 'b/emacs-lisp-mode-hook)

;;; YAML

(defvar b/yaml-mode-fmt-args '("-formatter" "indent=2,include_document_start=true,max_line_length=132"))
(make-variable-buffer-local 'b/yaml-mode-fmt-args)

(defun b/format-yaml (&optional display-errors)
  (interactive "P")
  (b/format-buffer-with
   "yamlfmt"
   :display-errors display-errors
   :args (cl-concatenate 'list b/yaml-mode-fmt-args '("-"))))

(defun b/yaml-mode-hook ()
  (setf tab-width 2)
  (add-hook 'before-save-hook #'b/format-yaml nil t)
  (flymake-mode))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.\\(yaml\\|yml\\)\\'")

(setf yaml-ts-mode-yamllint-options '("--config-data" "{rules: {line-length: {max: 150}}}"))

(add-hook 'yaml-ts-mode-hook #'b/yaml-mode-hook)

;;; Nix
(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode)
  :init
  (add-hook 'nix-mode-hook #'b/nix-mode-hook))

(defun b/format-nix (&optional display-errors)
  (interactive "P")
  (b/format-buffer-with
   "nixfmt"
   :display-errors display-errors
   :args (list "-f" (file-name-nondirectory (buffer-file-name)))))

(defun b/nix-mode-hook ()
  (add-hook 'before-save-hook #'b/format-nix nil t))

;;; Go

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(defun binarin/go-ts-mode-hook ()
  (eglot-ensure)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
  (electric-pair-local-mode t))

(add-hook 'go-ts-mode-hook 'binarin/go-ts-mode-hook)


(use-package info
  :ensure nil
  :defer t
  :defines (Info-current-file))

(defun b/Info-selection-hook ()
  (when (equal (file-name-nondirectory Info-current-file) "elisp")
    (add-hook 'xref-backend-functions 'elisp--xref-backend)))

(add-hook 'Info-selection-hook 'b/Info-selection-hook)

(defun b/tab-width-2 ()
  (setf tab-width 2))

(use-package kdl-mode
  :ensure t
  :mode ("\\.kdl\\'" . kdl-mode)
  :hook (kdl-mode . b/tab-width-2))

(defun b/rust-mode-hook ()
  (electric-pair-local-mode t))

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'". rust-ts-mode)
  :hook (rust-ts-mode . b/rust-mode-hook))


(cl-defstruct b/flake-subproject
  root-dir flake-dir)

(defun b/try-flake-subproject (dir)
  (let ((candidate (locate-dominating-file dir ".envrc")))
    (when (and candidate
	       (not (file-exists-p (file-name-concat candidate "flake.nix"))))
      (let ((flake (locate-dominating-file (file-name-parent-directory candidate) "flake.nix")))
	(when flake
	  (make-b/flake-subproject :root-dir candidate :flake-dir flake))))))

(defvar project-buffers-viewer)
(with-eval-after-load 'project
  (add-hook 'project-find-functions #'b/try-flake-subproject)
  (setf project-buffers-viewer 'project-list-buffers-ibuffer))

(bind-key "<f21>" 'project-eshell)


(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
	      ("<f23>" . eglot-code-actions)))


(cl-defmethod project-root ((project b/flake-subproject))
  (b/flake-subproject-root-dir project))

(cl-defmethod project-external-roots ((project b/flake-subproject))
  (list (b/flake-subproject-flake-dir project)))

(require 'project)

(cl-defmethod project-files ((project b/flake-subproject) &optional dirs)
  (let* ((expanded-dirs (or (and dirs (project-combine-directories dirs))
			    (list (b/flake-subproject-root-dir project))))
	 (command
	  (format
	   "fd --print0 --absolute-path . %s"
	   (string-join (mapcar (b/compose #'shell-quote-argument #'expand-file-name) expanded-dirs)
			" ")))
	 (files (string-split (shell-command-to-string command) "\0" t)))
    files))

(use-package view
  :ensure nil
  :commands (view-mode)
  :bind (:map view-mode-map
	      ("j" . 'next-line)
	      ("k" . 'previous-line)
	      ("n" . 'next-line)
	      ("p" . 'previous-line)))

(defun b/view-mode-for-nix-store ()
  (when (string-prefix-p "/nix/store/" (buffer-file-name))
    (view-mode 1)))

(add-hook 'find-file-hook 'b/view-mode-for-nix-store)


(defun b/indent-zmk-layer ()
  (interactive)
  (let ((indent-tabs-mode nil)
	beg end)
    (save-excursion
      (beginning-of-line)
      (while (not (looking-at (rx (* space) "bindings" (* space) "=" (* space) "<") t))
        (forward-line -1))
      (forward-line)
      (beginning-of-line)
      (setf beg (point))
      (re-search-forward ">;")
      (forward-line -1)
      (end-of-line)
      (setf end (point))
      (align-regexp beg end (rx (not (any " \n")) (group (+ " ")) (not (any " \n"))) 1 1 t))))

(use-package devicetree-ts-mode
  :ensure nil
  :defer t
  :mode "\\.keymap\\'"
  :bind (:map devicetree-ts-mode-map
	      ("C-c C-i" . b/indent-zmk-layer)))

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

(b/hide-ml-mode 'eldoc-mode)

(eval-when-compile
  (require 'eshell))

(with-eval-after-load 'eshell
  (add-to-list 'eshell-modules-list 'eshell-smart))

;;;###autoload
(defun b/rr (cmd)
  (interactive "sCommand name: ")
  (when-let* ((exe (file-truename (b/find-exe cmd))))
    (dired-jump t exe)))

(use-package haskell
  :ensure nil
  :commands (haskell-unicode-input-method-enable)
  :bind (:map haskell-mode-map
              ("C-c i" . haskell-navigate-imports)

              ("C-c C-M-c" . haskell-hide-toggle-all)
              :map interactive-haskell-mode-map
              ("C-c C-c" . haskell-hide-toggle)))

(defun b/haskell-mode-hook ()
  (haskell-unicode-input-method-enable)
  (add-hook 'xref-backend-functions #'etags--xref-backend nil t))

(add-hook 'haskell-mode-hook 'b/haskell-mode-hook)

(use-package prolog
  :ensure nil
  :config
  (setf prolog-electric-dot-flag t
        prolog-electric-dot-full-predicate-template t
        prolog-help-function-i 'prolog-find-documentation))

(provide 'b-prog-modes)
