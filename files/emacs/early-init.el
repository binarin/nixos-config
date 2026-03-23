;;; -*- mode: emacs-lisp; lexical-binding: t -*-
(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

(require 'cl-lib)
(require 'rx)
(require 'b-xdg)

(cl-loop
 for dir in (list b/xdg-state-home b/xdg-cache-home b/xdg-runtime-dir)
 unless (file-exists-p dir)
 do (make-directory dir t))

(defun binarin/locate-user-emacs-file (new-name &optional old-name)
  (ignore old-name)
  (let ((state-rx (rx bol
                      (or
		       "projects.eld"
		       "custom.el"
                       "history"
                       "recentf.eld"
                       "url"
		       "completions"
		       "network-security.eld"
		       "bookmarks.eld"
		       "eshell"
                       "transient"
		       "calc.el"
		       "org-clock-save.el"
		       "user-lisp"
		       "diary")
                      (or eol "/")))
        (cache-rx (rx bol
                      (or
                       "treesitter"
                       "eln"
		       "elpa"
		       "org-roam.db"
		       "init_zsh.sh"
		       "js/js"
		       ".org-id-locations")
                      (or eol "/")))
        (tmp-rx (rx bol
                    (or
                     "server") (or eol "/"))))
    (cl-flet* ((make-path (dir nm)
                 (convert-standard-filename (file-name-concat dir nm)))
               (state-file (nm)
                 (make-path b/xdg-state-home nm))
               (cache-file (nm)
                 (make-path b/xdg-state-home nm))
               (tmp-file (nm)
                 (make-path b/xdg-runtime-dir nm)))
      (when (listp new-name) (setf new-name (car new-name)))
      (cond
       ((string-match state-rx new-name) (state-file new-name))
       ((string-match cache-rx new-name) (cache-file new-name))
       ((string-match tmp-rx new-name) (tmp-file new-name))
       (t (warn "LUF: '%s'" new-name)
          (state-file new-name))))))

(advice-add 'locate-user-emacs-file :override #'binarin/locate-user-emacs-file)
(startup-redirect-eln-cache (binarin/locate-user-emacs-file "eln"))
