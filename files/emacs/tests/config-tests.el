;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)

(ert-deftest config-test-user-emacs-directory-clean ()
  (recentf-save-list)
  (let (unnecessary-files)
    (cl-loop for (file-name is-directory) in (directory-files-and-attributes user-emacs-directory)
	     unless (pcase file-name
		      ("init.el" t)
		      ("early-init.el" t)
		      ("lisp" t)
		      ("tests" t)
		      ("." t)
		      (".."t))
	     do (push file-name unnecessary-files))
    (should (equal nil unnecessary-files))))




