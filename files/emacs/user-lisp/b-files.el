;; -*- lexical-binding: t; -*-
(require 'b-xdg)
(require 'project)

(setf auto-save-list-file-prefix (file-name-concat b/xdg-runtime-dir))

(savehist-mode t)

;; Mostly do not want to track down which sensitive files should be excluded from backups
(setf make-backup-files nil)
(setf create-lockfiles nil)

(use-package tramp
  :ensure nil
  :defines (tramp-ssh-controlmaster-options)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setf tramp-ssh-controlmaster-options ""))

(use-package tramp-rpc
  :ensure nil
  :no-require t
  :defines (tramp-rpc-deploy-never-deploy)
  :config
  (setf tramp-rpc-deploy-never-deploy t))

(require 'recentf)
(setf recentf-max-saved-items 1000
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
  (dired-omit-mode 1)
  (keymap-local-set "M-k" #'b/dired-git-delete))

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

;;;###autoload
(defun b/copy-project-relative-filename ()
  "Kill the current buffer filename relative to the project root."
  (interactive)
  (if-let* ((project (project-current))
           (filename (buffer-file-name)))
      (let ((rel-name (file-relative-name filename (project-root project))))
        (kill-new rel-name)
        (message "Copied: %s" rel-name))
    (user-error "Not visiting a file in a project")))

(declare-function magit-file-delete "magit-files")
(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun b/dired-git-delete ()
  "Delete marked files (or file at point) via `magit-file-delete'.

After deletion, revert the dired buffer."
  (interactive nil dired-mode)
  (if-let* ((project (project-current)))
      (let* ((root (project-root project))
             (full-paths (dired-get-marked-files))
             (files (mapcar (lambda (f) (file-relative-name f root)) full-paths)))
        (magit-file-delete files)
        (dolist (f full-paths) (setq recentf-list (delete f recentf-list)))
        (revert-buffer))
    (user-error "Not in a project")))

(provide 'b-files)
