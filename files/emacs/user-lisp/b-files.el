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

(use-package recentf
  :ensure nil
  :defines (recentf-keep recentf-exclude)
  :init
  (setf recentf-max-saved-items 1000
        recentf-auto-cleanup 300
        recentf-show-messages nil)
  (setopt recentf-autosave-interval 180)
  :config
  ;; Keep remote/TRAMP files out of recentf — checking their
  ;; existence triggers noisy connection-attempt failures.
  (defun b/recentf-keep-p (file)
    "Return non-nil if FILE is a readable local file."
    (and (not (file-remote-p file))
         (file-readable-p file)))
  (setq recentf-keep '(b/recentf-keep-p))
  ;; Belt-and-suspenders: prevent remote files from being added.
  (add-to-list 'recentf-exclude #'file-remote-p)
  (recentf-mode t))

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
  "Kill the current buffer filename relative to the project root.
In Dired buffers, uses the file at point."
  (interactive)
  (if-let* ((project (project-current))
           (filename (or (buffer-file-name)
                         (and (derived-mode-p 'dired-mode)
                              (dired-file-name-at-point)))))
      (let ((rel-name (file-relative-name filename (project-root project))))
        (kill-new rel-name)
        (message "Copied: %s" rel-name))
    (user-error "Not visiting a file in a project")))

(declare-function magit-file-delete "magit-files")
(declare-function dired-get-marked-files "dired")
(declare-function dired-file-name-at-point "dired")

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
