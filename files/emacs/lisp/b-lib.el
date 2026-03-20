;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun b/strip-indentation (string)
  (let* ((lines (cdr (string-lines string)))
	 (indentation (string-match (rx (not " ")) (car lines)))
	 (strip-re (rx-to-string `(seq line-start (** 0 ,indentation " ")))))
    (string-join
     (cl-loop
      for line in lines
      collect (concat (string-trim-right
		       (replace-regexp-in-string strip-re "" line))
		      "\n")))))

(provide 'b-lib)
