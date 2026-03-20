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

(setf yaml-ts-mode-yamllint-options '("--config-data" "{rules: {line-length: {max: 132}}}"))

(add-hook 'yaml-ts-mode-hook #'b/yaml-mode-hook)

;;; Nix
(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode)
  :config
  (add-hook nix-mode-hook #'b/nix-mode-hook))

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

(provide 'b-prog-modes)
