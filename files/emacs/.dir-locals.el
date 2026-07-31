;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((compile-command . (concat "cd " (project-root (project-current)) " && ./scripts/test-emacs.sh")))))
