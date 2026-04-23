;; -*- lexical-binding: t; -*-
(cl-eval-when (compile load eval)
  (provide 'org-mouse))

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


(defvar b/anki-capture-note-type "Basic")
(defvar b/anki-capture-note-target nil)

;;;###autoload
(defun b/anki-capture-location (&optional select)
  (interactive "P")
  (if (or (equal select '(4))
          (not (markerp b/anki-capture-note-target)))
      (let ((org-agenda-files nil)
            (org-refile-targets '((("~/org/anki.org") . (:maxlevel . 2)))))
        (org-refile '(4))
        (org-fold-reveal '(4))
        (setf b/anki-capture-note-target (point-marker)))
    (org-goto-marker-or-bmk b/anki-capture-note-target)
    (org-fold-reveal '(4))))

(defmacro b/pipe (expr &rest forms)
  (declare (indent 1))
  `(let ((it ,expr))
     ,@(cl-loop
        for form in forms
        if (functionp form)
        collect `(setf it (funcall ,form it))
        collect `(setf it ,form))
     it))

(defun b/org-capture-init-indent-stripped (&optional new-prefix)
  (when-let* ((init (org-capture-get :initial))
              (stripped (b/strip-indentation (concat "\n" init))))
    (when new-prefix
      (setf
       stripped
       (b/pipe stripped
         (string-split it "\n")
         (mapcar (lambda (ln) (concat new-prefix ln)) it)
         (string-join it "\n"))))
    stripped))

(defvar b/anki-basic-card-template
  `("anki-editor card" entry (function b/anki-capture-location)
    ,(b/strip-indentation "
        * %?
          :PROPERTIES:
          :ANKI_NOTE_TYPE: %(progn b/anki-capture-note-type)
          :capture-location: %a
          :capture-timestamp: %U
          :END:

        %(b/org-capture-init-indent-stripped \"  \")
      ")
    :hook org-fold-hide-drawer-all
    :prepare-finalize b/org-remove-empty-properties-from-capture
    :after-finalize b/org-capture-fold-after))

(setf org-capture-templates
      `(("a" ,@b/anki-basic-card-template)
        ("t" "TODO" entry (file "~/org/refile.org")
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
	 "  - %U %i"
	 :immediate-finish t)
	("r" "Add note to an interactively selected task"
         plain (function b/org-goto-heading-and-go-inside)
	 "  - %U %?")
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
	(cl-loop for file in '("personal.org" "refile.org" "ference.org" "maybe.org" "caldav.org" "booking.org")
		 for full = (file-name-concat "~/org" file)
		 if (file-exists-p full)
		 collect full)))
(setf org-refile-targets '((org-agenda-files . (:maxlevel . 5))))

(setf org-agenda-window-setup 'other-tab
      org-agenda-restore-windows-after-quit t
      org-agenda-todo-ignore-with-date t)

(setf org-agenda-custom-commands
      `(("w" agenda ""
	 ((org-agenda-start-day "-1d")
	  (org-agenda-start-on-weekday nil)))
        ("a"
         ((agenda ""
                  ((org-agenda-span 'day)))
          (tags-todo "-agenda_hide-REFILE+TODO=\"NEXT\""
                     ((b/agenda-prepend-parent t)
                      (org-agenda-overriding-header "Tasks:")
                      (org-agenda-hide-tags-regexp ,(rx (or "PROJ")))
                      (org-agenda-sorting-strategy '(priority-down category-keep))))
          (stuck "")
          (tags "REFILE/-DONE-CNCL"
                ((org-agenda-overriding-header "Items to refile:"))))
         ((org-agenda-compact-blocks t)))))

(setf org-stuck-projects '("+PROJ-agenda_hide/+TODO" ("NEXT")))
(setf org-tags-exclude-from-inheritance '("PROJ"))
(setf org-tags-column -118)
(setf org-ellipsis "…")

(setf org-export-with-sub-superscripts '{}
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil)


(setf org-lowest-priority ?D
      org-highest-priority ?A
      org-default-priority ?C)

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
  "m" (b/org-roam-refile-without-id)
  "M" (org-roam-extract-subtree))

(setf org-protocol-default-template-key "l")

(require 'org-roam)
(require 'org-roam-dailies)

(defun b/org-roam-format (node)
  (if (eql 0 (org-roam-node-level node))
      (org-roam-node-title node)
    (format "%s %s"
            (propertize (format "[%s]" (org-roam-node-file-title node))
                        'face 'org-agenda-dimmed-todo-face)
            (org-roam-node-title node))))
(setf org-roam-node-display-template 'b/org-roam-format)

(defun b/org-roam-refile-without-id ()
  (interactive)
  (when-let* ((target (org-roam-node-read nil nil nil 'require-match)))
    (org-delete-property "ID")
    (org-roam-refile target)))

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
	               (delete-frame ,frame-var t))
	           99 t)))))

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
  (save-excursion
    (if-let*  ((_ (re-search-forward org-link-any-re (pos-eol) t))
               (_ (org-element-type-p
                   (save-match-data (org-element-context))
                   '(link comment comment-block node-property keyword)))
               (link (match-string 0)))
        (org-link-open-from-string link)
      (message "No link in heading."))))

;;;###autoload
(defvar b/org-capture-gui-selection--body)
(defun b/org-capture-gui-selection ()
  ;; (gui-selection-value) doesn't work on wayland, when emacs doesn't have focus
  ;; XXX it's kinda interesting that `wl-paste` hangs when emacs HAS focus.
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

(require 'org-caldav)
(setf org-caldav-url "https://nextcloud.lynx-lizard.ts.net/remote.php/dav/calendars/binarin"
      org-caldav-calendar-id "binarin"
      org-caldav-inbox "~/org/caldav.org"
      org-icalendar-timezone "Europe/Amsterdam"
      org-caldav-files (cl-set-difference org-agenda-files '("~/org/archive.org" "~/org/caldav.org") :test #'equal)
      org-caldav-save-directory "~/org")

;;;###autoload
(defun b/org-clock-in-select ()
  (interactive)
  (org-clock-in '(4)))


(autoload 'consult-org-agenda "consult-org")

;;;###autoload
(defun b/org-goto-heading ()
  (interactive)
  (consult-org-agenda))

;;;###autoload
(defun b/org-add-note-to-selected ()
  (interactive)
  (org-capture nil "r"))

(defun b/full-frame-add-note-to-selected ()
  (b/with-org-capture-frame
    (b/org-add-note-to-selected)))

(defun b/org-goto-heading-and-go-inside ()
  (b/org-goto-heading)
  (forward-line 1))

;;;###autoload
(defun b/org-clock-out ()
  (interactive)
  (org-clock-out)
  (force-mode-line-update t))



(defvar b/agenda-prepend-parent nil)

(defun b/agenda-item-prepend-parent (item)
  (let ((marker (org-find-text-property-in-string 'org-marker item)))
    (when (markerp marker)
      (org-with-point-at marker
        (when-let* ((parent (and (= 3 (org-current-level))
                                 (org-up-heading-safe)
                                 (org-entry-get nil "ITEM")))
                    (project-text (propertize (concat "[" parent "] ")
                                              'face 'org-agenda-dimmed-todo-face)))
          (setf item (replace-regexp-in-string
                      (rx (or "NEXT" "WAIT") " " (? (regexp org-priority-regexp)) (group-n 10) (* anychar) eos)
                      project-text
                      item nil nil 10)))))
    item))

(defun b/org-agenda-finalize-prepend-parent (string)
  (if (not b/agenda-prepend-parent)
      string
    (string-join
     (mapcar 'b/agenda-item-prepend-parent
             (split-string string "\n" 'omit-nulls))
     "\n")))

(advice-add 'org-agenda-finalize-entries :filter-return 'b/org-agenda-finalize-prepend-parent)

(defvar b/org-clock-heading-file
  (file-name-concat (or (getenv "XDG_RUNTIME_DIR") "/tmp")
                    "org-mode/org-mode-clock.txt"))

(defun b/org-clock-write-heading-to-file (val)
  (with-temp-buffer
    (insert val)
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) b/org-clock-heading-file))))

(defun b/org-clock-remove-heading-file ()
  (when (file-exists-p b/org-clock-heading-file)
    (delete-file b/org-clock-heading-file)))

(defun b/org-clock-heading-watcher (sym newval operation where)
  (ignore sym where)
  (when (eq operation 'set)
    (b/org-clock-write-heading-to-file newval)))

(add-variable-watcher 'org-mode-line-string 'b/org-clock-heading-watcher)


(add-hook 'org-clock-out-hook 'b/org-clock-remove-heading-file)

(setf org-clock-persist 'history)
(org-clock-persistence-insinuate)
(add-hook 'org-clock-out-hook 'org-clock-save)


(setf org-clock-auto-clockout-timer 600)
(org-clock-auto-clockout-insinuate)

(provide 'b-org)
