;; -*- lexical-binding: t; -*-
(require 'flymake)
(require 'flymake-proc)
(require 'prolog)
(require 'cl-lib)

(require 'b-comint)

;;;###autoload
(defun b/prolog-config ()
  (bind-key '[remap prolog-consult-file] #'b/prolog-consult-file)
  (add-hook 'prolog-mode 'b/prolog-mode-hook)
  (add-hook 'prolog-inferior-mode 'b/prolog-inferior-mode-hook)
  (setf prolog-system 'swi
        prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O"))
                                  (t nil))
        prolog-electric-if-then-else-flag t
        prolog-electric-dot-flag t
        prolog-electric-colon-flag t
        prolog-electric-dot-full-predicate-template nil
        prolog-help-function-i 'prolog-find-documentation))

(defun b/prolog-mode-hook ()
  (b/prolog-setup-flymake-backend)
  (setq-local flymake-show-diagnostics-at-end-of-line t)
  (flymake-mode 1))

(defun b/prolog-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'b/prolog-flymake))

(defun b/prolog-inferior-mode-hook ()
  (b/comint-persist-history-setup ".prolog-comint.history"))

(defun b/prolog-consult-file ()
  "Consult file of current buffer."
  (interactive)
  (save-some-buffers t)
  (let ((display-buffer-alist '((".*" (display-buffer-no-window) (allow-no-window t)))))
    (prolog-consult-compile-file nil))

  ;; prolog compilation-mode setup is a bit strange, as the output comes from inferior prolog process.

  ;; prolog-consult-compile-file is semi-synchronous - when it
  ;; returns, the consulting is already done/all content is inserted
  ;; into the compilation buffer. But compilation parsing things are
  ;; not done yet, so we can't just pick compilation-num-errors-found.
  (let* ((buffer (get-buffer "*prolog-compilation*"))
         (errors (with-current-buffer buffer
                   (save-match-data
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "ERROR:" nil t))))))
    (if errors
        (display-buffer buffer)
      (message "Consulted."))))

(defvar-local b/prolog--flymake-proc nil)

(defun b/prolog-flymake (report-fn &rest _args)
  (unless (executable-find "swipl")
    (error "Cannot find swipl binary"))
  (when (process-live-p b/prolog--flymake-proc)
    (kill-process b/prolog--flymake-proc))
  (let* ((source (current-buffer))
         (temp-file-name (flymake-proc-create-temp-inplace (buffer-file-name source) "flymake")))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file-name)
      (setf
       b/prolog--flymake-proc
       (make-process
        :name "prolog-flymake"
        :noquery t
        :connection-type 'pipe
        :buffer (generate-new-buffer " *prolog-flymake*")
        :command `("swipl" "-t" "halt" "-s" ,temp-file-name)
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (if (with-current-buffer source (eq proc b/prolog--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp
                              (rx bol
                                  (group-n 1 (or "ERROR" "Warning"))
                                  ": "
                                  (group-n 2 (+? nonl))
                                  ":"
                                  (group-n 3 (+ digit))
                                  ":"
                                  (group-n 4 (+ digit))
                                  ": "
                                  (group-n 5 (+ nonl))
                                  eol)
                              nil t)
                       for msg = (match-string 5)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 3))
                                          (string-to-number (match-string 4)))
                       for type = (if (string= "ERROR" (match-string 1))
                                      :error
                                    :warning)
                       when (and beg end)
                       collect (flymake-make-diagnostic source beg end type msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Cancelling obsolete check %s" proc))
              (delete-file temp-file-name)
              (kill-buffer (process-buffer proc)))))))
      (process-send-eof b/prolog--flymake-proc))))

(provide 'b-prolog)
