;; -*- lexical-binding: t; -*-
(require 'subr-x)

;;;; Seat-level idle via wprintidle-c (Wayland ext-idle-notify-v1)
;;
;; Talks to the wprintidle-c daemon over its Unix socket so that
;; `org-user-idle-seconds' can report real seat-level idle instead of
;; Emacs-only idle under Wayland compositors where logind IdleHint is
;; never driven.  See ../../../../docs/superpowers/specs/2026-07-22-wprintidle-org-clockout-design.md.

(defun b/wprintidle-socket-path ()
  "Return the wprintidle-c daemon socket path.
Mirrors the daemon's path logic in its `common.c':
`$XDG_RUNTIME_DIR/wprintidle-c.sock', falling back to
`/tmp/wprintidle-c-<uid>.sock' when `XDG_RUNTIME_DIR' is unset."
  (let ((dir (getenv "XDG_RUNTIME_DIR")))
    (if (and dir (not (string-empty-p dir)))
        (expand-file-name "wprintidle-c.sock" dir)
      (format "/tmp/wprintidle-c-%d.sock" (user-uid)))))

(defun b/parse-wprintidle-response (text)
  "Parse a wprintidle-c daemon response.
TEXT is the raw string returned by the socket.  Returns the idle
time in seconds as a float (0.0 when the user is active), or nil
if TEXT is empty or not a non-negative integer."
  (let ((trimmed (string-trim text)))
    (when (and (not (string-empty-p trimmed))
               (string-match-p "\\`[0-9]+\\'" trimmed))
      (float (string-to-number trimmed)))))

(provide 'b-wprintidle)
