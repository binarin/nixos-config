;; -*- lexical-binding: t; -*-
(require 'b-xdg)

(setf auto-save-list-file-prefix (file-name-concat b/xdg-runtime-dir))

(savehist-mode t)

;; Mostly do not want to track down which sensitive files should be excluded from backups
(setf make-backup-files nil)

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

(provide 'b-files)
