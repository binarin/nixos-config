;; -*- lexical-binding: t; -*-

(defvar b/tests-generated-files-dir (make-temp-file "emacs-config-generated" t))
(setf user-emacs-directory (make-temp-file "emacs-config-user" t))

(message "%s\n%s" b/tests-generated-files-dir user-emacs-directory)

(advice-add 'locate-user-emacs-file
	    :override
	    (lambda (new-name &optional old-name)
	      (ignore old-name)
	      (when (listp new-name)
		(setf new-name (car new-name)))
	      (convert-standard-filename (file-name-concat b/tests-generated-files-dir new-name))))

(startup-redirect-eln-cache (locate-user-emacs-file "eln"))

