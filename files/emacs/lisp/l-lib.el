;; -*- lexical-binding: t; -*-

(defun load? (n)
  (unless (featurep n)
    (load (symbol-name n))))

(provide 'l-lib)
