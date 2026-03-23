;; -*- lexical-binding: t; -*-

(defun b/ripgrep (needle)
  (interactive "sFixed string: ")
  (compilation-start (format (concat
			      "rg --smart-case --fixed-strings --with-filename "
			      "--no-heading " 
			      "--color always --colors path:none --colors line:none --colors match:fg:cyan "
			      "--max-columns=300 --max-columns-preview --null "
			      "--regexp=%s ")
			     (shell-quote-argument needle))
		     #'b/ripgrep-mode))

(defvar b/ripgrep-match-regexp (rx "\033[36m" (group (+ (not "\033")))))

(defvar-local b/ripgrep-num-matches-found)

(defun b/ripgrep-process-setup ()
  (setf b/ripgrep-num-matches-found 0))

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
	  (incf b/ripgrep-num-matches-found))
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

(defvar b/ripgrep-error-regexp-alist
  `((,(rx bol
	  (group-n 1 (+ (not (any "\0\n"))))
	  (group-n 3 "\0")
	  (group-n 2 (+ (any digit)))
	  ":")
     1 2
     ,(cons #'b/ripgrep--col-beg #'b/ripgrep--col-end)
     nil nil
     (3 '(face nil display ":")))))

(define-compilation-mode b/ripgrep-mode "Ripgrep"
  ""
  (require 'grep)
  ;; (setf b/ripgrep-num-matches-found 0)
  (setq-local compilation-process-setup-function #'b/ripgrep-process-setup)
  (setq-local compilation-disable-input t)
  ;; (setq-local compilation-hidden-output "[\0]") ;; XXX
  (setq-local compilation-error-face grep-hit-face)
  (setq-local compilation-error-regexp-alist
	      b/ripgrep-error-regexp-alist)
  (unless kill-transform-function
    (setq-local kill-transform-function #'identity))
  (add-function :filter-return (local 'kill-transform-function)
		(lambda (string)
		  (string-replace "\0" ":" string)))
  (add-hook 'compilation-filter-hook #'b/ripgrep-filter nil t))

(provide 'b-ripgrep)
