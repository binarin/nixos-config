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

(defun b/wayland-idle-seconds ()
  "Return seat-level idle seconds under Wayland, via wprintidle-c.
Opens the daemon's Unix socket directly, sends `QUERY_SECONDS', and
reads one line.  Returns a float, or nil if the daemon is
unavailable, the connection times out, or the response is invalid.
Never signals."
  (when (featurep 'make-network-process '(:family local))
    (let ((path (b/wprintidle-socket-path))
          (buf (generate-new-buffer " *wprintidle*"))
          proc)
      (unwind-protect
          (condition-case nil
              (progn
                (setq proc
                      (make-network-process
                       :name "wprintidle"
                       :buffer buf
                       :family 'local
                       :service path
                       :noquery t
                       :sentinel (lambda (_p _e))))
                (process-send-string proc "QUERY_SECONDS\n")
                ;; Wait up to ~0.1s for the response line.
                (with-local-quit
                  (accept-process-output proc 0.1))
                (when (process-live-p proc)
                  (with-current-buffer buf
                    (b/parse-wprintidle-response
                     (buffer-string)))))
            ;; Any error (connection refused, etc.) -> nil.
            (error nil))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'b-wprintidle)