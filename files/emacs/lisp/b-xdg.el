;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(defvar b/xdg-app "emacs-clean")
(defvar b/xdg-state-home (file-name-concat
			  (or (getenv "XDG_STATE_HOME")
			      (expand-file-name "~/.local/state"))
			  b/xdg-app))
(defvar b/xdg-cache-home (file-name-concat
			  (or (getenv "XDG_CACHE_HOME")
			      (expand-file-name "~/.cache"))
			  b/xdg-app))
(defvar b/xdg-runtime-dir (file-name-concat
			   (or (getenv "XDG_RUNTIME_DIR")
			       (progn
				 (warn "No XDG_RUNTIME_DIR, defaulting to ~/tmp")
				 (expand-file-name "~/tmp"))
			       )
			   b/xdg-app))

(provide 'b-xdg)
