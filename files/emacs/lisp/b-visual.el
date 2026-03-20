;; -*- lexical-binding: t; -*-

(context-menu-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font-16"))

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


(provide 'b-visual)
