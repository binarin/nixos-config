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


;;;###autoload
(defun b/display-buffer-from-dedicated-frame (buffer alist)
  "When used from within a dedicated frame, displays buffer in the previous
frame. So e.g. when compilation is targeted to a dedicated frame,
clicking on an error message will most likely show error location in the
previously selected frame."
  (let ((display-buffer-overriding-action nil))
    (when-let* ((_ (frame-parameter nil 'b/dedicated-frame))
                (target-window (next-window nil nil 'visible))
                (target-frame (window-frame target-window))
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
