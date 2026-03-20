;; -*- lexical-binding: t; -*-
(defvar b/format-buffer-with-error-buffer)
(make-variable-buffer-local 'b/format-buffer-with-error-buffer)

(cl-defun b/format-buffer-with (prog &key display-errors args)
  (let ((stdout-file (make-temp-file "*fmt out"))
	(stderr-file (make-temp-file "*fmt err"))
	stderr-buffer)
    (unwind-protect
      (save-restriction
	(widen)
	(let ((exit-code
	       (apply #'call-process-region nil nil ;; start, end
				    prog
				    nil ;; delete
				    (list (list :file stdout-file) stderr-file) ;; buffer
				    nil ;; display
				    args)))
	  (if (and (numberp exit-code) (eql exit-code 0))
	      (progn
		(insert-file-contents stdout-file nil nil nil t)
		(when (buffer-live-p b/format-buffer-with-error-buffer)
		  (delete-windows-on b/format-buffer-with-error-buffer)
		  (setf b/format-buffer-with-error-buffer nil)))
	    (progn
	      (unless (buffer-live-p b/format-buffer-with-error-buffer)
		(setf b/format-buffer-with-error-buffer
		      (generate-new-buffer (format "%s errors" (buffer-file-name)) t)))
	      (setf stderr-buffer b/format-buffer-with-error-buffer)
	      (with-current-buffer b/format-buffer-with-error-buffer
		(let ((inhibit-read-only t))
		  (insert-file-contents stderr-file nil nil nil t))
		(compilation-mode)
		(if display-errors
		    (display-buffer stderr-buffer)
		  (message "'%s' failed: see '%s'" prog stderr-buffer)))))))
      (delete-file stderr-file)
      (delete-file stdout-file))))

(provide 'b-format-on-save)
