;; -*- lexical-binding: t; -*-
;; (defun ghostel-send-C-k-and-kill ()
;;     "Send `C-k' to ghostel.
;; Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
;;     (interactive)
;;     (kill-ring-save (point) (line-end-position))
;;     (ghostel-send-key "k" "ctrl"))


(use-package ghostel
  :ensure nil
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("M-<backspace>" . ghostel-backward-kill-word)
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package ghostel-eshell
  :ensure nil
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-compile
  :ensure nil
  :hook (after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :ensure nil
  :hook (after-init . ghostel-comint-global-mode))


(provide 'b-ghostel)
