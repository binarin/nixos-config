;; -*- lexical-binding: t; -*-
(setf enable-recursive-minibuffers t)

;; Hide commands in M-x which do not work in the current mode.  Vertico
;; commands are hidden in normal buffers. This setting is useful beyond
;; Vertico.
(setf read-extended-command-predicate #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setf minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))


;;; Completion:
(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil)

(use-package vertico-multiform
  :ensure nil
  :after vertico)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(setf vertico-multiform-categories
      '((file (:keymap . vertico-directory-map))))


(keymap-set vertico-directory-map "C-l" #'vertico-directory-delete-word)
;; XXX - not working, picking DEL from global map
;; (keymap-set vertico-directory-map "DEL" #'vertico-directory-delete-char)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(use-package corfu-history :ensure nil :after corfu)
(use-package corfu-popupinfo :ensure nil :after corfu)

(use-package corfu
  :ensure t
  :commands (global-corfu-mode)
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
  :commands (marginalia-mode)
  :init
  (marginalia-mode t))

(use-package embark
  :ensure t
  :bind (("C-;" . embark-act))
  :commands (embark-context-menu)
  :init
  (add-hook 'context-menu-functions #'embark-context-menu 100)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(use-package embark-consult
  :ensure t)

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

(defun b/prog-mode-setup-dabbrev ()
  (setq-local dabbrev-case-replace nil
	      dabbrev-case-fold-search nil))

(add-hook 'prog-mode-hook #'b/prog-mode-setup-dabbrev)

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

(vertico-multiform-mode)

(provide 'b-completion)
