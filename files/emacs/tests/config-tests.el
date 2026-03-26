;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'recentf)

(ert-deftest config-test-user-emacs-directory-clean ()
  (recentf-save-list)
  (let (unnecessary-files)
    (cl-loop for (file-name) in (directory-files-and-attributes user-emacs-directory)
	     unless (pcase file-name
		      (".dir-locals.el" t)
		      ("init.el" t)
		      ("early-init.el" t)
		      ("tests" t)
		      ("user-lisp" t)
		      ("." t)
		      (".."t))
	     do (push file-name unnecessary-files))
    (should (equal nil unnecessary-files))))


