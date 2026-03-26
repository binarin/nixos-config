;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((compile-command . (format "cd %s && ./scripts/test-emacs.sh"
					        (project-root (project-current)))))))
