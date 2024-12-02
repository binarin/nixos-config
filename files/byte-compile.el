;;; -*- lexical-binding: t -*-
(setf kill-emacs-hook nil) ;; some packages can populate this during load time with non-pure stuff, we have no reason to run it

(let (src dst dst-early compilation-result dst-dir)
  (setf src (pop argv))
  (setf dst-dir (pop argv))

  (setf dst (expand-file-name "init.el" dst-dir))
  (setf dst-early (expand-file-name "early-init.el" dst-dir))

  (package-initialize)
  (require 'ob-tangle)

  (cl-letf (((symbol-function 'org-babel-effective-tangled-filename)
             (lambda (buffer-fn src-lang src-tfile)
               (ignore buffer-fn src-lang)
               (if (string-equal "early-init.el" src-tfile)
                   dst-early
                 dst))))
    (org-babel-tangle-file src nil "emacs-lisp"))

  (setq make-backup-files nil) ;; we change buffers below, inhibit backups

  (dolist (el-name (list dst dst-early))
    (setq )
    (let ((buf (find-file-noselect el-name t)))
      (with-current-buffer buf
        (beginning-of-buffer)
        (insert ";;; -*- lexical-binding: t -*-\n")
        (save-buffer))
      (kill-buffer buf)))

  (setf byte-compile-error-on-warn t)
  (setf compilation-result (and
                            (byte-compile-file dst)
                            (byte-compile-file dst-early)))

  (unless compilation-result
    (kill-emacs 1)))
