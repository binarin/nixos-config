;; -*- lexical-binding: t; -*-
(require 'b-xdg)

(setf auto-save-list-file-prefix (file-name-concat b/xdg-runtime-dir))

(savehist-mode t)

;; Mostly do not want to track down which sensitive files should be excluded from backups
(setf make-backup-files nil)

(use-package tramp
  :ensure nil
  :defines (tramp-ssh-controlmaster-options)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setf tramp-ssh-controlmaster-options ""))

(require 'recentf)
(setf recentf-max-saved-items 200
      recentf-auto-cleanup 300
      recentf-show-messages nil)
(setopt recentf-autosave-interval 180)
(recentf-mode t)

(save-place-mode t)
(setopt save-place-autosave-interval 180)

(setopt dired-isearch-filenames 'dwim
	dired-dwim-target 'dired-dwim-target-recent
	wdired-allow-to-change-permissions t
	dired-hide-details-hide-symlink-targets nil)

(autoload 'dired-omit-mode "dired-x")
(defun b/dired-mode-hook ()
  (toggle-truncate-lines t)
  (dired-omit-mode 1))

(add-hook 'dired-mode-hook 'b/dired-mode-hook)

(eval-when-compile
  (require 'auth-source-pass))
(setf auth-source-pass-filename (expand-file-name "~/.local/share/gopass/stores/root/"))
(with-eval-after-load 'auth-source
    (auth-source-pass-enable))

;;;  If the text under point looks like a hostname – say,
;;;  something.com in a comment – ffap tries to ping it to check if
;;;  it’s reachable. On a slow or firewalled network, that’s a
;;;  multi-second hang.
(use-package ffap
  :ensure nil
  :init
  (ffap-bindings)
  (setq ffap-machine-p-known 'reject))

(provide 'b-files)
