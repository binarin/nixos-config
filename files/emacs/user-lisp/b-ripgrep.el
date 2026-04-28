;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'grep)
(require 'compile)
(require 'project)

(defgroup b/ripgrep nil
  "Run ripgrep"
  :group 'tools
  :group 'processes)

(defvar b/ripgrep-base-args
  '("rg"
    "--with-filename" ;; show filename even if only one file is searched
    "--null" ;; use \0 to separate filename from line number - don't need to handle file-names with ':' in them

    "--color" "ansi" 
    "--colors" "path:none"
    "--colors" "line:none"
    "--colors" "match:fg:cyan" ;; we'll match it via b/ripgrep-match-regexp
    )
  "Mandatory flags that are necessary for the rest of the code here to
function properly")

(defvar b/ripgrep-match-regexp (rx "\033[36m" (group (+ (not "\033"))))
  "`cyan' from b/ripgrep-base-args")

(defvar b/ripgrep-use-headings t)

(defvar b/ripgrep-history nil)

(defvar compilation-filter-start)

(defun b/ripgrep-process-setup ()
  (setf grep-num-matches-found 0))

(defun b/ripgrep-filter ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setf beg (point))
      (when (< (point) end) ;; should pick the whole lines only
	(setf end (copy-marker end))
	(while (re-search-forward b/ripgrep-match-regexp end 1)
	  (replace-match (propertize (match-string 1)
				     'face nil 'font-lock-face grep-match-face)
			 t t)
	  (incf grep-num-matches-found))
	(when b/ripgrep-use-headings
	  (replace-string-in-region "\0" "\0\n" beg end))
	;; clean up the rest of escapes
	(goto-char beg)
	(while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
	  (replace-match "" t t))))))

(defun b/ripgrep--col-beg ()
  (let* ((beg (match-end 0))
	 (end (save-excursion (goto-char beg) (line-end-position)))
	 (mbeg (text-property-any beg end 'font-lock-face grep-match-face)))
    (when mbeg
      (- mbeg beg))))

(defun b/ripgrep--col-end ()
  (let* ((beg (match-end 0))
	 (end (save-excursion (goto-char beg) (line-end-position)))
	 (mbeg (text-property-any beg end 'font-lock-face grep-match-face))
	 (mend (and mbeg (next-single-property-change mbeg 'font-lock-face nil end))))
    (when mend
      (- mend beg))))


(defvar b/ripgrep-error-regexp-alist-alist
  `((rg-file-line
     ,(rx bol
	  (? (group-n 1 (+ (not (any "\0\n"))))
	     (group-n 3 "\0"))
	  (group-n 2 (+ (any digit)))
	  ":")
     1 2
     ,(cons #'b/ripgrep--col-beg #'b/ripgrep--col-end)
     2 ;; file with line number -> error
     nil
     (3 '(face nil display ":")))
    (rg-file-only
     ,(rx bol (group-n 1 (+ (not (any "\0\n")))) (group-n 3 "\0"))
     1 nil nil
     1 ;; file without line number(heading) -> warning
     nil
     (3 '(face nil display "")))
    (rg-binary-file
     ,(rx bol (group-n 1 (+? not-newline)) ": binary file matches")
     1 nil nil
     0 ;; binary file -> error
     1)
    (rg-line-only
     ,(rx bol (group-n 2 (+ digit)) ":")
     nil 2
     ,(cons #'b/ripgrep--col-beg #'b/ripgrep--col-end)
     2 ;; line number without file -> error
     )))

(cl-defun b/ripgrep-command (pattern
			     &key
			     (case-sensitive 'smart) ;; nil/smart/t
			     (pattern-type 'fixed) ;; fixed/default/pcre2/auto
			     follow
			     (heading t)
			     search-zip
			     invert-match
			     (binary t)
			     sort ;; nil=none/path/modified/accessed/created
			     sort-reverse
			     (max-columns 300)
			     (max-columns-preview t))
  (let (parts)
    (cl-loop for arg in b/ripgrep-base-args
	     do (push arg parts))

    (push (pcase case-sensitive
	    ('smart "--smart-case")
	    ('nil "--ignore-case")
	    ('t "--case-sensitive")
	    (unk (error "unknown case-sensitive: '%s'" unk)))
	  parts)
    
    (if (eq pattern-type 'fixed)
	(push "--fixed-strings" parts)
      (push "--engine" parts)
      (push (symbol-name pattern-type) parts))

    ;; bool flags, like --follow/--no-follow
    (cl-loop for (flag value) in `(("follow" ,follow) ("heading" ,heading) ("search-zip"  ,search-zip)
				   ("invert-match" ,invert-match) ("max-columns-preview" ,max-columns-preview)
				   ("binary" ,binary))
	     do (if value
		    (push (concat "--" flag) parts)
		  (push (concat "--no-" flag) parts)))
    
    (when sort
      (push (if sort-reverse "--sortr" "--sort") parts)
      (push (symbol-name sort) parts))

    (cl-loop for (flag value) in `(("max-columns" ,max-columns))
	     do (push (format "--%s" flag) parts)
	     do (push (format "%s" value) parts))

    (push "--regexp" parts)
    (push pattern parts)

    (setf parts (nreverse parts))
    (string-join (mapcar #'shell-quote-argument parts) " ")))


(defvar b/ripgrep-mode)
(define-compilation-mode b/ripgrep-mode "Ripgrep"
  :syntax-table nil
  :abbrev-table nil
  (setq-local compilation-disable-input t)
  (toggle-truncate-lines t)

  (setq-local compilation-skip-threshold (if b/ripgrep-use-headings
					     2 ;; only show errors, i.e. files 
					   0))
  (setq-local compilation-mode-line-errors grep-mode-line-matches)
  (setq-local compilation-process-setup-function #'b/ripgrep-process-setup)

  (setq-local compilation-error-face grep-hit-face)
  (setq-local compilation-warning-face grep-hit-face)

  (setq-local compilation-error-regexp-alist-alist b/ripgrep-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
	      (if b/ripgrep-use-headings
		  '(rg-file-only rg-binary-file rg-line-only)
		'(rg-file-line rg-binary-file)))

  (unless kill-transform-function
    (setq-local kill-transform-function #'identity))
  (add-function :filter-return (local 'kill-transform-function)
		(lambda (string)
		  (string-replace "\0" ":" string)))

  (setq-local ansi-color-for-compilation-mode nil)
  (add-hook 'compilation-filter-hook #'b/ripgrep-filter nil t))


(cl-defun b/ripgrep (needle &rest command-args &key name-function &allow-other-keys)
  (interactive
   (let ((pattern (read-from-minibuffer "Ripgrep: " nil nil nil 'b/ripgrep-history)))
     (list pattern)))
  (compilation-start (apply #'b/ripgrep-command needle :heading b/ripgrep-use-headings
                            (map-delete command-args :name-function))
		     #'b/ripgrep-mode
                     name-function))

(defvar b/ripgrep-main-target "/rpc:murmur:/usr/local/git_tree/main")
(cl-defun b/ripgrep-main (needle)
  (interactive "sPattern: ")
  (let ((default-directory b/ripgrep-main-target))
    (b/ripgrep needle
               :name-function #'(lambda (_mode) (format "*ripgrep-main: %s*" needle))
               :pattern-type 'auto)))

(defun b/ripgrep-project (project)
  (interactive
   (list (project-current t)))
  (let ((default-directory (project-root project)))
    (call-interactively #'b/ripgrep)))



(provide 'b-ripgrep)
