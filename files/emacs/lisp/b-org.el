;; -*- lexical-binding: t; -*-
(require 'b-visual)
(require 'org)

(setf org-startup-folded 'show2levels
      org-fold-catch-invisible-edits 'smart
      org-hide-block-startup t)

(setf org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "|" "CNCL(c)")
	(sequence "WAIT" "|")))

(b/with-zenburn
  (setf org-todo-keyword-faces
	`(("TODO" . ,zenburn-red+1)
	  ("NEXT" . ,zenburn-cyan)
	  ("DONE" . ,zenburn-green)
	  ("WAIT" . ,zenburn-orange)
	  ("CNCL" . ,zenburn-fg-05))))



(provide 'b-org)
