;; -*- lexical-binding: t; -*-
(defun b/color-luminance (color)
  (cl-destructuring-bind (r g b) (mapcar (lambda (c) (/ c 256.0)) (color-values color))
    (/ (+ (* .2126 r) (* .7152 g) (* .0722 b)) 255)))

(defun b/colorize-hex ()
  (b/with-zenburn
    (let* ((color (match-string-no-properties 0))
	  (contrasting (if (< 0.5 (b/color-luminance color))
			   zenburn-bg
			 zenburn-fg)))
      (put-text-property
       (match-beginning 1) (match-end 1)
       'face `((:background ,color :foreground ,contrasting)))
      (put-text-property
       (match-beginning 2) (match-end 2)
       'face `((:foreground ,color :background ,contrasting))))))


(defvar b/rainbow-keywords
  (list (list (rx "#" (group (= 3 (in "0-9" "A-F" "a-f"))) (group (= 3 (in "0-9" "A-F" "a-f"))))
	      '(0 (b/colorize-hex)))))

(define-minor-mode b/rainbow-mode
  "Colorizes #XXXXXX colors in a buffer"
  :lighter nil
  (progn
    (if b/rainbow-mode
	(font-lock-add-keywords nil b/rainbow-keywords t)
      (font-lock-remove-keywords nil b/rainbow-keywords))
    (font-lock-mode 1)))

(provide 'b-rainbow)
