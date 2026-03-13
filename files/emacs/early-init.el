;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(require 'cl-lib)
(require 'rx)
(defun binarin/locate-user-emacs-file (new-name &optional old-name)
  (let ((xdg-state-home (or (getenv "XDG_STATE_HOME")
                            (expand-file-name "~/.local/state")))
        (xdg-cache-home (or (getenv "XDG_CACHE_HOME")
                            (expand-file-name "~/.cache")))
        (xdg-runtime-dir (or (getenv "XDG_RUNTIME_DIR")
                             (error "Runtime dir not known")))
        (state-rx (rx bol
                      (or
                       "history"
                       "recentf.eld"
                       "url"
                       "transient")
                      (or eol "/")))
        (cache-rx (rx bol
                      (or
                       "treesitter"
                       "eln")
                      (or eol "/")))
        (tmp-rx (rx bol
                    (or
                     "server") (or eol "/"))))
    (cl-flet* ((make-path (dir nm)
                 (convert-standard-filename (file-name-concat dir "emacs-clean" nm)))
               (state-file (nm)
                 (make-path xdg-state-home nm))
               (cache-file (nm)
                 (make-path xdg-state-home nm))
               (tmp-file (nm)
                 (make-path xdg-runtime-dir nm)))
      (when (listp new-name) (setf new-name (car new-name)))
      (cond
       ((string-match state-rx new-name) (state-file new-name))
       ((string-match cache-rx new-name) (cache-file new-name))
       ((string-match tmp-rx new-name) (tmp-file new-name))
       (t (warn "LUF: '%s'" new-name)
          (state-file new-name))))))

(advice-add 'locate-user-emacs-file :override #'binarin/locate-user-emacs-file)

(startup-redirect-eln-cache (binarin/locate-user-emacs-file "eln"))
