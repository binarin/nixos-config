;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(declare-function niri-rpc-connected-p "niri-rpc")
(declare-function niri-frame-niri-id "niri-frame")
(declare-function niri-rpc-focus-window "niri-rpc")

(defun b/dedicated-frame-p (frame)
  (frame-parameter frame 'b/dedicated-frame))

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




;;;###autoload
(defun b/other-window-backward (&optional count)
  "Like `b/other-window', but move in the opposite direction.
COUNT defaults to 1; pass it to `b/other-window' negated."
  (interactive "p")
  (b/other-window (- (or count 1))))

(defvar-keymap b/other-window-repeat-map
  :doc "Repeat map for `b/other-window'.  Used in `repeat-mode'."
  :repeat t
  "o" #'b/other-window
  "O" #'b/other-window-backward)

(defun b/visible-frames ()
  "Frames considered on-screen by the niri-aware `frame-visible-p'.
`frame-visible-p' is advised by `track-niri-frame-visibility-mode'
to return nil for frames on inactive (scrolled-off) niri
workspaces.  The selected frame is always included, even when
reported off-screen, so navigation has a place to start from."
  (let* ((cur (selected-frame))
         (vis (cl-remove-if-not #'frame-visible-p (frame-list))))
    (if (memq cur vis) vis (cons cur vis))))

(defun b/window-cycle ()
  "Stable ordered list of selectable windows across `b/visible-frames'.
Frames are taken in `frame-list' order; within each frame, windows
in the frame's natural cyclic order from its first window.  Windows
for which `window-no-other-p' returns non-nil are dropped.

The order does NOT depend on which window is selected, so the whole
cycle is a fixed ring: callers move through it by advancing an index
from the selected window (see `b/other-window').  Rotating the ring
to begin at the selected window instead would restart the local
frame's windows first on every call, trapping navigation inside a
multi-window frame and never reaching another frame.

This reimplements, for a niri-aware frame set, what `other-window'
gets from `next-window' — necessary because `next-window' is a C
primitive whose ALL-FRAMES argument only accepts nil / t /
`visible' / 0 / a single frame (a list means \"this frame only\"),
so it cannot be told to cycle a curated cross-frame set."
  (cl-remove-if
   #'window-no-other-p
   (cl-mapcan
    (lambda (f) (window-list f nil (frame-first-window f)))
    (b/visible-frames))))

;;;###autoload
(defun b/other-window (&optional count)
  "Select another window among visible (niri-aware) frames only.
Cycles the windows returned by `b/window-cycle', which spans every
frame `frame-visible-p' reports on-screen — so it crosses to
frames on other outputs while skipping frames on inactive niri
workspaces.  COUNT (default 1) is the number of windows to move,
negative to move backward; the cycle wraps.

This does its own cross-frame cycling instead of delegating to
`other-window', because `other-window' / `next-window' evaluate
ALL-FRAMES in C and ignore the `frame-visible-p' advice, and a
frame *list* passed as ALL-FRAMES is treated as \"current frame
only\" — so neither `visible' nor an explicit list produces the
niri-aware cross-frame behavior we want.

When the niri IPC connection is live and the selected frame
changed as a result, sync Wayland keyboard focus to the new frame
by sending a `FocusWindow' action for its niri window id (if
known)."
  (interactive "p")
  (let* ((count (or count 1))
         (before-frame (selected-frame))
         (start (selected-window))
         (windows (b/window-cycle))
         (n (length windows))
         (idx (cl-position start windows)))
    (when (and (> n 0) idx)
      (let ((target (nth (mod (+ idx count) n) windows)))
        (unless (eq target start)
          (select-window target)
          (when (and (niri-rpc-connected-p)
                     (not (eq before-frame (selected-frame))))
            (when-let* ((id (niri-frame-niri-id (selected-frame))))
              (niri-rpc-focus-window id))))))))

(put 'b/other-window 'repeat-map 'b/other-window-repeat-map)
(put 'b/other-window-backward 'repeat-map 'b/other-window-repeat-map)

(provide 'l-windows)
