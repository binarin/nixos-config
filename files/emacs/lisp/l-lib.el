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

(provide 'l-lib)
