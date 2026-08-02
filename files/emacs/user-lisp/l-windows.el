;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(declare-function niri-rpc-connected-p "niri-rpc")
(declare-function niri-frame-niri-id "niri-frame")
(declare-function niri-rpc-focus-window "niri-rpc")
(defvar niri-frame-visible-inhibit) ; defined in niri-frame-visible.el

;;;###autoload
(defun b/dedicated-frame-p (frame)
  (frame-parameter frame 'b/dedicated-frame))

(defun b/another-toplevel-frame-p (frame)
  "Return non-nil if FRAME has a usable sibling it can fall back to.

Mirrors the other-frame test inside `handle-delete-frame' (a
visible, non-child frame with no `delete-before' parameter, other
than FRAME), but evaluates visibility with the niri-awareness
override on `frame-visible-p' disabled.

`track-niri-frame-visibility-mode' advises `frame-visible-p' to
report frames on inactive niri workspaces as invisible.  Left in
place, that advice would make `handle-delete-frame' believe no
other frame exists when the only siblings live on other
workspaces — so closing the last frame on the active workspace
would be escalated to `save-buffers-kill-emacs' (and then vetoed
by `emacs-lock-mode', stranding the frame).  Binding
`niri-frame-visible-inhibit' for this scan restores the intended
meaning: any live sibling, on any workspace, counts."
  (let ((niri-frame-visible-inhibit t))
    (catch 'other-frame
      (dolist (other (frame-list))
        (when (and (not (eq other frame))
                   (frame-visible-p other)
                   (not (frame-parent other))
                   (not (frame-parameter other 'delete-before)))
          (throw 'other-frame t))))))

(defun b/handle-delete-frame-1 (frame)
  "Decide what a WM close of FRAME should do.
If FRAME has a sibling it can fall back to (per
`b/another-toplevel-frame-p'), delete just FRAME; otherwise signal
Emacs to quit via `save-buffers-kill-emacs'.

This is the routing core factored out of the
`b/handle-delete-frame-around' advice so it can be tested without
constructing a real input event."
  (if (b/another-toplevel-frame-p frame)
      (delete-frame frame t)
    (save-buffers-kill-emacs)))

;;;###autoload
(defun b/handle-delete-frame-around (_orig event &rest _args)
  "Around advice for `handle-delete-frame'.

ORIG is the original `handle-delete-frame'; EVENT is the
`delete-frame' event from the window system.  Routes the close
through `b/handle-delete-frame-1', which uses niri-unaware
visibility so siblings on other workspaces keep a close from
being escalated to a full Emacs quit."
  (b/handle-delete-frame-1 (posn-window (event-start event))))

;;;###autoload
(defun b/maybe-install-handle-delete-frame-advice ()
  "Install the niri-aware `handle-delete-frame' advice, once.
Idempotent; safe to call from an idle timer after the niri frame
visibility mode may have been enabled."
  (unless (advice-member-p #'b/handle-delete-frame-around
                           'handle-delete-frame)
    (advice-add 'handle-delete-frame :around
                #'b/handle-delete-frame-around)))

(b/maybe-install-handle-delete-frame-advice)

;;;###autoload
(defun b/display-buffer-use-dedicated-frame (buffer alist)
  (when-let* ((window (display-buffer-use-some-frame
                       buffer
                       (cl-list* '(inhibit-switch-frame . t)
                                 '(frame-predicate . b/dedicated-frame-p)
                                 alist))))
    (delete-other-windows)
    window))

;;;###autoload
(defun b/make-frame-dedicated (&optional prefix)
  (interactive "P")
  (dolist (frame (frame-list))
    (modify-frame-parameters frame '((b/dedicated-frame . nil))))

  (pcase prefix
    ('(4) (when-let* ((buffer (read-buffer "Use the frame of buffer: "))
                      (window (get-buffer-window buffer 'visible))
                      (frame (window-frame window)))
            (modify-frame-parameters frame '((b/dedicated-frame . t)))))
    ('(16)) ;; keep cleared by dolist above
    (_ (modify-frame-parameters nil '((b/dedicated-frame . t))))))


(defvar b/last-non-dedicated-frame nil
  "The last selected frame that is not a dedicated frame.
Updated by `select-frame-functions' whenever a non-dedicated
frame is selected.  Used by `b/display-buffer-from-dedicated-frame'
to redirect display to the frame the user was last working in.")

(defun b/track-last-non-dedicated-frame (frame)
  "Remember FRAME if it is not a dedicated frame."
  (unless (frame-parameter frame 'b/dedicated-frame)
    (setq b/last-non-dedicated-frame frame)))

(add-hook 'select-frame-functions #'b/track-last-non-dedicated-frame)

;;;###autoload
(defun b/display-buffer-from-dedicated-frame (buffer alist)
  "When used from within a dedicated frame, displays buffer in the previous
frame. So e.g. when compilation is targeted to a dedicated frame,
clicking on an error message will most likely show error location in the
previously selected frame."
  (let ((display-buffer-overriding-action nil))
    (when-let* ((_ (frame-parameter nil 'b/dedicated-frame))
                (visible-non-dedicated
                 (cl-remove-if-not
                  (lambda (f)
                    (and (not (frame-parameter f 'b/dedicated-frame))
                         (frame-visible-p f)))
                  (visible-frame-list)))
                (target-frame
                 (cond
                  ;; No visible non-dedicated frames: use dedicated itself
                  ((null visible-non-dedicated)
                   (selected-frame))
                  ;; Exactly one: use it directly, no disambiguation needed
                  ((null (cdr visible-non-dedicated))
                   (car visible-non-dedicated))
                  ;; Multiple visible non-dedicated frames: use the
                  ;; last one the user was working in
                  (t
                   (or (and b/last-non-dedicated-frame
                            (frame-live-p b/last-non-dedicated-frame)
                            (not (frame-parameter
                                  b/last-non-dedicated-frame
                                  'b/dedicated-frame))
                            (frame-visible-p b/last-non-dedicated-frame)
                            b/last-non-dedicated-frame)
                       (car visible-non-dedicated)))))
                (pred (lambda (frame) (equal frame target-frame))))
      (display-buffer-use-some-frame buffer
                                     (cl-list* `(frame-predicate . ,pred)
                                               alist)))))

;;;###autoload
(defun b/display-buffer-from-dedicated-frame-around-advice (orig &rest args)
  (if (frame-parameter nil 'b/dedicated-frame)
      (let ((display-buffer-overriding-action '(b/display-buffer-from-dedicated-frame)))
        (apply orig args))
    (apply orig args)))

(provide 'l-windows)
