;; -*- lexical-binding: t; -*-
(require 'avy)

;;;###autoload
(defun b/avy-window-list-visible-frame-advice (orig)
  (pcase avy-all-windows
    ('all-visible-frames
     (cl-mapcan #'window-list (cl-remove-if-not #'frame-visible-p (frame-list))))
    (_ (funcall orig))))

(provide 'l-avy)
