;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun load? (n)
  (unless (featurep n)
    (load (symbol-name n))))

(defun b/compose (&rest functions)
  (cl-assert (car functions))
  (lambda (&rest args)
    (cl-loop with fns = (reverse functions)
	     with val = (apply (pop fns) args)
	     for next-fn in fns
	     do (funcall next-fn val)
	     finally return val)))

(defun b/find-exe (exe)
  (if (file-name-absolute-p exe)
      exe
    (cl-loop for path in exec-path
	     for full-path = (file-name-concat path exe)
	     when (file-executable-p full-path)
	     return full-path)))

(defun b/hide-ml-mode (mode)
  (add-to-list 'mode-line-collapse-minor-modes mode))


(provide 'l-lib)
