;; -*- lexical-binding: t; -*-
(use-package magit
  :ensure t
  :commands (magit-file-relative-name magit-git-string)
  :bind (("C-x g" . magit)))

;;;###autoload
(defun b/open-forge-link ()
  (interactive)
  (when-let* ((remote-url (magit-git-string "remote" "get-url" "origin"))
              (_ (string-match (rx bol "git@" (group-n 1 (+ (not ":"))) ":" (group-n 2 (* nonl)) eol)
                               remote-url))
              (forge (match-string 1 remote-url))
              (project (string-remove-suffix ".git" (match-string 2 remote-url)))
              (rev (magit-git-string "rev-parse" "HEAD"))
              (file-prefix
               (pcase forge
                 ("github.com" (format "blob/%s" rev))
                 ("gitlab.com" (format "-/blob/%s" rev))
                 ("forgejo.lynx-lizard.ts.net" (format "src/commit/%s" rev))))
              (line-range (if (region-active-p)
                              (format "#L%d-L%d"
                                      (line-number-at-pos (region-beginning))
                                      (line-number-at-pos (1- (region-end))))
                            (format "#L%d" (line-number-at-pos)))))
    (browse-url-xdg-open (format "https://%s/%s/%s/%s%s"
                                 forge project file-prefix
                                 (magit-file-relative-name (buffer-file-name))
                                 line-range))))

(setf (alist-get (rx bol "magit-diff:") display-buffer-alist nil nil #'equal)
      '(b/display-buffer-use-dedicated-frame))

;;;###autoload
(defun b/project-delete-file-and-kill-buffer ()
  "Delete the current buffer's file (via `magit-file-delete') and kill the buffer.

If the buffer has unsaved changes, asks for confirmation first.
If the file has uncommitted git changes, Magit will prompt."
  (interactive)
  (if-let ((filename (buffer-file-name))
           (project (project-current)))
      (when (or (not (buffer-modified-p))
                (y-or-n-p (format "File %s has unsaved changes; discard? "
                                  (file-relative-name filename (project-root project)))))
        (set-buffer-modified-p nil)
        (magit-file-delete (list filename))
        (kill-buffer))
    (user-error "Not visiting a file in a project")))

(provide 'b-version-control)
