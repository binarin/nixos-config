;; -*- lexical-binding: t; -*-

(setf inhibit-startup-screen t)

(require 'emacs-lock)

(defun b-startup--wayland-env-p ()
  "Return non-nil when running as a graphical Wayland session."
  (and (getenv "NIRI_SOCKET")
       (display-graphic-p)))

(defun b-startup--lock-scratch ()
  "Enable `emacs-lock-mode' in the *scratch* buffer under Wayland."
  (when (and (b-startup--wayland-env-p)
             (equal (buffer-name) "*scratch*"))
    (emacs-lock-mode 'exit)))

(defun b-startup--lock-org-file ()
  "Enable `emacs-lock-mode' for personal.org/refile.org when visited."
  (when (member (file-name-nondirectory (buffer-file-name))
                '("personal.org" "refile.org"))
    (emacs-lock-mode 'exit)))

;; Prevent accidental Emacs exits by locking selected buffers. While a
;; buffer is exit-locked, Emacs refuses to quit.
(b-startup--lock-scratch)
(add-hook 'find-file-hook #'b-startup--lock-org-file)

(defun b/really-kill-emacs (&optional arg)
  "Kill Emacs, bypassing `emacs-lock-mode' exit locks.
ARG is passed to `kill-emacs' (which see)."
  (interactive "P")
  (let ((kill-emacs-hook
         (delq #'emacs-lock--kill-emacs-hook
               (copy-sequence kill-emacs-hook)))
        (kill-emacs-query-functions
         (delq #'emacs-lock--kill-emacs-query-functions
               (copy-sequence kill-emacs-query-functions))))
    (kill-emacs arg)))

(provide 'b-startup)
