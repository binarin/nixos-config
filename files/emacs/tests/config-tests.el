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



(require 'b-wprintidle)

(ert-deftest b-org-test-wprintidle-socket-path-xdg ()
  "Socket path uses $XDG_RUNTIME_DIR when set."
  (let ((process-environment (cons "XDG_RUNTIME_DIR=/run/user/12345"
                                   process-environment)))
    (should (equal "/run/user/12345/wprintidle-c.sock"
                   (b/wprintidle-socket-path)))))

(ert-deftest b-org-test-wprintidle-socket-path-tmp-fallback ()
  "Socket path falls back to /tmp/wprintidle-c-<uid>.sock without XDG_RUNTIME_DIR."
  (let ((process-environment
         (cl-remove-if (lambda (e) (string-prefix-p "XDG_RUNTIME_DIR=" e))
                       process-environment)))
    (should (equal (format "/tmp/wprintidle-c-%d.sock" (user-uid))
                   (b/wprintidle-socket-path)))))

(ert-deftest b-org-test-parse-wprintidle-response-seconds ()
  "A numeric line parses to a float."
  (should (equal 42.0 (b/parse-wprintidle-response "42\n"))))

(ert-deftest b-org-test-parse-wprintidle-response-active ()
  "\"0\\n\" means active user -> 0.0, not nil."
  (should (equal 0.0 (b/parse-wprintidle-response "0\n"))))

(ert-deftest b-org-test-parse-wprintidle-response-garbage ()
  "Non-numeric / empty input returns nil."
  (should (equal nil (b/parse-wprintidle-response "")))
  (should (equal nil (b/parse-wprintidle-response "garbage\n"))))
