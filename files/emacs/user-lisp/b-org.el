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
(require 'org-mouse)

(setf org-startup-folded 'show2levels
      org-startup-shrink-all-tables t
      org-fold-catch-invisible-edits 'smart
      org-hide-block-startup t
      org-log-into-drawer "LOGBOOK")

(setf org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
	(sequence "|" "CNCL(c@)")
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

(setf org-clock-in-switch-to-state "NEXT")

(setf org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)


(setf org-archive-location "~/org/archive.org::* From %s"
      org-archive-subtree-save-file-p t)

(setf org-default-notes-file (concat org-directory "/notes.org"))

(require 'notifications)


(setf org-capture-templates
      `(("t" "TODO" entry (file "~/org/refile.org")
	 ,(b/strip-indentation "
           * %?
             :PROPERTIES:
             :ID: %(org-id-new)
             :capture-clocked: %K
             :capture-location: %a
             :capture-timestamp: %U
             :END:

             %i
          ")
	 :hook org-fold-hide-drawer-all
	 :prepare-finalize b/org-remove-empty-properties-from-capture
	 :after-finalize b/org-capture-fold-after
	 )
	("s" "Start working on a task" entry (file "~/org/refile.org")
	 ,(b/strip-indentation "
           * NEXT %?
             :PROPERTIES:
             :ID: %(org-id-new)
             :capture-clocked: %K
             :capture-location: %a
             :capture-timestamp: %U
             :END:

             %i
          ")
	 :clock-in t
	 :clock-keep t
	 :hook org-fold-hide-drawer-all
	 :prepare-finalize b/org-remove-empty-properties-from-capture
	 :after-finalize b/org-capture-fold-after
	 )
	("z" "Add note to a running clock" plain (clock)
	 "  - %U %?")
	("Z" "Add note to a running clock (pre-fill/immediate finish)" plain (clock)
	 "  - %U %i "
	 :immediate-finish t)
	("l" "Capture link" entry (file "~/org/refile.org")
	 ,(b/strip-indentation "
           * %a
             :PROPERTIES:
             :ID: %(org-id-new)
             :capture-clocked: %K
             :capture-timestamp: %U
             :END:

             %i
          ")
	 :immediate-finish t
	 :prepare-finalize
	 (lambda ()
	   (b/org-remove-empty-properties-from-capture)
	   (ignore-errors
	     (notifications-notify :title "Link captured"
				   :body (caar org-stored-links)))))
	("P" "Capture clipboard contents" entry (file "~/org/refile.org")
	 ,(b/strip-indentation "
           * %i
             :PROPERTIES:
             :ID: %(org-id-new)
             :capture-clocked: %K
             :capture-timestamp: %U
             :END:

             %(progn b/org-capture-gui-selection--body)
          ")
	 :immediate-finish t
	 :prepare-finalize
	 (lambda ()
	   (b/org-remove-empty-properties-from-capture)
	   (ignore-errors
	     (notifications-notify :title "Clipboard captured"
				   :body (org-get-heading)))))))

(defun b/org-capture-fold-after ()
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (let ((pt (org-capture-get :insertion-point)))
	(when pt
	  (org-with-point-at pt
	    (pcase (org-get-property-block pt)
	      (`(,_ . ,_)
     	       (save-restriction
		 (when (re-search-forward org-property-drawer-re)
		   (goto-char (match-beginning 0))
		   (org-fold-hide-drawer-toggle t)))))
	    (org-fold-hide-entry)))))))

(setf org-agenda-files
      (when (file-exists-p "~/org")
	(cl-loop for file in '("personal.org" "refile.org" "ference.org" "maybe.org" "caldav.org")
		 for full = (file-name-concat "~/org" file)
		 if (file-exists-p full)
		 collect full)))
(setf org-refile-targets '((org-agenda-files . (:maxlevel . 5))))

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

(require 'org-keys)
(setf org-use-speed-commands t)

(defun b/set-org-speed-command (key form)
  (setf (alist-get key org-speed-commands) form))

(defmacro b/set-org-speed-commands (&rest pairs)
  (declare (indent 0))
  `(progn
     ,@(cl-loop for (key form) on pairs by #'cddr
		collect `(b/set-org-speed-command ,key ',form))))

(b/set-org-speed-commands
  "a" (org-archive-subtree-default)
  "d" (org-todo 'done)
  "c" (org-todo "CNCL")
  "J" (org-refile '(4))
  "i" (org-clock-in)
  "k" (progn)
  "o" (b/org-open-first-link-at-point)
  "m" (org-roam-refile (org-roam-node-read nil nil nil 'require-match))
  "M" (org-roam-extract-subtree))

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

(defun b/org-remove-empty-properties-from-capture ()
  (interactive)
  (cl-loop for property in '("capture-clocked" "capture-location")
	   for val = (org-entry-get (point) property)
	   when (equal "" val)
	   do (org-entry-delete nil property)))

(defun b/clock-out-on-screen-lock ()
  (when (org-clocking-p)
    (org-clock-out)
    (org-save-all-org-buffers)))

;;;###autoload
(defun b/org-goto-last-capture ()
  (interactive)
  (org-goto-marker-or-bmk org-capture-last-stored-marker))

;;;###autoload
(defun b/org-save-and-push-files ()
  (interactive)
  (save-some-buffers t)
  (let* ((default-directory "~/org")
	 (buffer-name "*Pushing org changes*")
	 (display-buffer-alist `((,(rx bol (literal buffer-name) eol)
				  (display-buffer-no-window)))))
    (message "Syncing org mode changes.")
    (async-shell-command "./push.sh" buffer-name)))


(defmacro b/with-org-capture-frame (&rest body)
  "BODY should leave org-capture buffer as current"
  (declare (indent 0))
  (let ((frame-var (gensym)))
    `(let  ((,frame-var (make-frame '((name . "*org-capture - Emacs(float)*"))))
	    (display-buffer-alist (cons (list (rx bol "CAPTURE-")
					      '(display-buffer-full-frame))
					display-buffer-alist)))
       (with-selected-frame ,frame-var
	 ,@body
	 (add-hook 'kill-buffer-hook
		   #'(lambda ()
		       (message "Want to clean-up")
		       (delete-frame ,frame-var t))
		   nil t)))))

;;;###autoload
(defun b/full-frame-org-capture (&optional keys)
  (b/with-org-capture-frame
    (when (symbolp keys)
      (setf keys (symbol-name keys))) ;; easier quoting in niri config, doesn't support raw strings in KDL
    (org-capture nil keys)))

;;;###autoload
(defun b/full-frame-org-roam-dailes-capture-today ()
  (b/with-org-capture-frame
    (org-roam-dailies-capture-today)))

;;;###autoload
(defun b/full-frame-org-note-on-running-clock ()
  (when (org-clocking-p)
    (b/with-org-capture-frame
      (org-capture nil "z"))))

;;;###autoload
(defun b/new-emacs-frame ()
  (make-frame))

;;;###autoload
(defun b/full-frame-org-agenda ()
  (let ((org-agenda-window-setup 'other-frame))
    (org-agenda nil "a")
    (setq-local org-agenda-window-setup 'other-frame)))

(defun b/org-open-first-link-at-point ()
  (interactive)
  (if-let* ((link (org-offer-links-in-entry (current-buffer) (point) 1)))
      (progn
	(org-link-open-from-string (car link)))))

;;;###autoload
(defvar b/org-capture-gui-selection--body)
(defun b/org-capture-gui-selection ()
  ;; (gui-selection-value) doesn't work on wayland, when emacs doesn't have focus
  (let* ((selection (shell-command-to-string "wl-paste -p")) 
	 (lines (string-split (string-trim selection) "\n"))
	 (heading (string-trim-left (car lines))))

    (setf b/org-capture-gui-selection--body
	  (when (cdr lines)
	    (string-join
	     (mapcar (lambda (s) (concat "  " s)) lines) ; repeat the first line in the body too
	     "\n")))
    
    (org-capture-string heading "P")))

;;;###autoload
(defun b/org-add-note-to-clocked ()
  (interactive)
  (if  (not (org-clocking-p))
      (error "No running clock to comment on")
    (org-capture-string (read-from-minibuffer "Note for clocked task: ") "Z")))

(provide 'b-org)

;;;;###autoload
(defun b/org-clock-in-select ()
  (interactive)
  (org-clock-in '(4)))


(provide 'b-org)
