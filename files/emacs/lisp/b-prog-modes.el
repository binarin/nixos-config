;; -*- lexical-binding: t; -*-
(require 'b-format-on-save)

(use-package cperl-mode
  :ensure nil
  :mode ("\\.\\(pl\\|pm\\)\\'"))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("RET" . paredit-newline)
	      ("C-j". nil)))


;;; emacs-lisp
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

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'". rust-ts-mode)
  :hook (rust-ts-mode . b/rust-mode-hook))

(defun b/rust-mode-hook ()
  (electric-pair-local-mode t))
(cl-defstruct b/flake-subproject
  root-dir flake-dir)

(defun b/try-flake-subproject (dir)
  (let ((candidate (locate-dominating-file dir ".envrc")))
    (when (and candidate
	       (not (file-exists-p (file-name-concat candidate "flake.nix"))))
      (let ((flake (locate-dominating-file (file-name-parent-directory candidate) "flake.nix")))
	(when flake
	  (make-b/flake-subproject :root-dir candidate :flake-dir flake))))))

(with-eval-after-load 'project
    (add-hook 'project-find-functions #'b/try-flake-subproject))

(bind-key "<f21>" 'project-eshell)


(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
	      ("<f23>" . eglot-code-actions)))


(cl-defmethod project-root ((project b/flake-subproject))
  (b/flake-subproject-root-dir project))

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



(provide 'b-prog-modes)
