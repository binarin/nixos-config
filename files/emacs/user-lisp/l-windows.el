;; -*- lexical-binding: t; -*-

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




(provide 'l-windows)
