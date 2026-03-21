;; -*- lexical-binding: t; -*-
(require 'org)
(require 'org-capture)
(require 'org-tempo)
(require 'cl-lib)
(require 'org-protocol)
(require 'org-clock)
(require 'org-archive)
(require 'org-agenda)
(require 'b-lib)
(require 'b-visual)
(require 'ox)

(setf org-startup-folded 'show2levels
      org-startup-shrink-all-tables t
      org-fold-catch-invisible-edits 'smart
      org-hide-block-startup t
      org-log-into-drawer "LOGBOOK")

(setf org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
	(sequence "|" "CNCL(c!)")
	(sequence "WAIT(w!/!)" "|")))

(b/with-zenburn
  (setf org-todo-keyword-faces
	`(("TODO" . ,zenburn-red)
	  ("NEXT" . ,zenburn-cyan)
	  ("DONE" . ,zenburn-green-3)
	  ("WAIT" . ,zenburn-orange)
	  ("CNCL" . ,zenburn-fg-05))))

(setf org-fast-tag-selection-single-key t
      org-tag-alist '(("PROJ" . ?p)))

(setf org-clock-idle-time 10)

(setf org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)


(setf org-archive-location "~/org/archive.org::* From %s"
      org-archive-subtree-save-file-p t)

(setf org-default-notes-file (concat org-directory "/notes.org"))

(setf org-capture-templates
      `(("t" "TODO" entry (file "~/org/refile.org")
	 ,(b/strip-indentation "
           * %?
             :PROPERTIES:
             :ID: %(org-id-new)
             :capture-location: %a
             :capture-timestamp: %U
             :END:

             %i
          ")
	 :hook org-fold-hide-drawer-all)
	("l" "Capture link" entry (file "~/org/refile.org")
	 ,(b/strip-indentation "
           * %a
             :PROPERTIES:
             :ID: %(org-id-new)
             :capture-timestamp: %U
             :END:

             %i
          ")
	 :immediate-finish t)))

(setf org-agenda-files
      (when (file-exists-p "~/org")
	(cl-loop for file in '("personal.org" "refile.org" "ference.org" "maybe.org" "caldav.org")
		 for full = (file-name-concat "~/org" file)
		 if (file-exists-p full)
		 collect full)))

(setf org-agenda-window-setup 'only-window
      org-agenda-restore-windows-after-quit t
      org-agenda-todo-ignore-with-date t)

(setf org-agenda-custom-commands
      '(("a" agenda ""
	 ((org-agenda-start-day "-1d")
	  (org-agenda-start-on-weekday nil)))))

(setf org-export-with-sub-superscripts '{}
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil)


(setf org-src-window-setup 'current-window
      org-src-ask-before-returning-to-edit-buffer nil)

(add-to-list 'org-structure-template-alist '("m" . "SRC emacs-lisp"))

(setf org-use-speed-commands t)
(setf org-protocol-default-template-key "l")

(require 'org-roam)
(require 'org-roam-dailies)

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M:%S>: %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))


(setf org-roam-directory (file-truename "~/org/roam"))
(setf org-roam-dailies-directory "daily/")

(when (file-exists-p "~/org/roam")
  (org-roam-db-autosync-mode))

(provide 'b-org)
