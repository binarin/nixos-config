;; -*- lexical-binding: t; -*-
(use-package treesit :ensure nil
  :config
  (setf treesit-font-lock-level 4))

(context-menu-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(autoload 'scroll-bar-mode "scroll-bar")

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font-16"))

(when (display-graphic-p)
  (set-face-attribute 'mode-line nil :height 0.8)
  (set-face-attribute 'mode-line-inactive nil :height 0.8))
(line-number-mode 1)
(column-number-mode 1)

(defun b/zenburn-all-colors ()
  (append zenburn-default-colors-alist
	  zenburn-override-colors-alist))

(defmacro b/with-zenburn (&rest body)
  (declare (indent 0))
  (let ((full-alist (mapcar (lambda (cons)
			      (cons (intern (car cons)) (cdr cons)))
			    (append zenburn-default-colors-alist
				    zenburn-override-colors-alist))))
    `(let (,@ (mapcar (lambda (cons)
			(list (car cons) (cdr cons)))
		      full-alist))
       ,@(mapcar (lambda (cons) (list 'ignore (car cons))) full-alist)
       ,@body)))

(defun b/zenburn-demo ()
  (interactive)
  (b/with-zenburn
    (cl-flet* ((center-pad (str length)
		 (let* ((str-len (length str))
			(rem-len (max 0 (- length str-len)))
			(pre-len (/ rem-len 2))
			(post-len (- rem-len pre-len)))
		   (concat (string-pad "" pre-len) str (string-pad "" post-len))))
	       (ins-block (text &key bg fg (width 8))
		 (let (face)
		   (when fg
		     (push (list :foreground fg) face))
		   (when bg
		     (push (list :background bg) face))
		   (insert (propertize (center-pad text width)
 				       'face face)))))
      (with-current-buffer (get-buffer-create "*zenburn demo*")
	(display-buffer (current-buffer))
	(erase-buffer)
	(cl-loop for (name . color) in (b/zenburn-all-colors)
		 do (ins-block color :width 8)
		 do (ins-block name :fg color :width 18)
		 do (ins-block name :bg color :fg zenburn-bg :width 18)
		 do (ins-block "bg-1" :fg color :bg zenburn-bg-1)
		 do (ins-block "fg-1" :bg color :fg zenburn-fg-1)
		 do (ins-block "bg+1" :fg color :bg zenburn-bg+1)
		 do (ins-block "fg+1" :bg color :fg zenburn-fg+1)
		 do (insert "\n"))
	(view-mode)))))

;; disable legacy behaviour when 'C-x C-x' highlights region, even if nothing was highlighted
(setf exchange-point-and-mark-highlight-region nil)

(provide 'b-visual)
