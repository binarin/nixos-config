;; -*- lexical-binding: t; -*-

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

;;;###autoload
(defun b/other-window (&optional count)
  "Select another window, cycling visible windows only.
Calls `other-window' with COUNT (default 1) and the symbol
`visible' as ALL-FRAMES, so minimized/iconified frames are
skipped.  When the niri IPC connection is live
\(`niri-rpc-connected-p') and the selected frame changed as a
result, sync Wayland keyboard focus to the new frame by sending
a `FocusWindow' action for its niri window id (if known).
No focus sync is attempted when the frame did not change or
when `niri-frame-niri-id' returns nil."
  (interactive "p")
  (let ((before-frame (selected-frame)))
    (other-window (or count 1) 'visible)
    (when (and (niri-rpc-connected-p)
               (not (eq before-frame (selected-frame))))
      (when-let* ((id (niri-frame-niri-id (selected-frame))))
        (niri-rpc-focus-window id)))))

(put 'b/other-window 'repeat-map 'b/other-window-repeat-map)
(put 'b/other-window-backward 'repeat-map 'b/other-window-repeat-map)

(provide 'l-windows)
