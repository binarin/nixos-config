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
              (project (match-string 2 remote-url))
              (rev (magit-git-string "rev-parse" "HEAD"))
              (file-prefix
               (pcase forge
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

(provide 'b-version-control)
