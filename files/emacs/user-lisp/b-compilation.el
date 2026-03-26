;; -*- lexical-binding: t; -*-
(require 'l-lib)
(require 'cl-lib)

(global-set-key (kbd "<f16>") 'recompile)
(global-set-key (kbd "C-<f16>") 'kill-compilation)
(global-set-key (kbd "M-<f16>") 'compile)

(setf compilation-ask-about-save nil)

(defvar b/systemd-run-args
  '("--user" "--scope" "--quiet"
    "--property" "ManagedOOMSwap=kill"
    "--property" "CPUWeight=idle"
    "--property" "MemoryHigh=50%"
    "--property" "MemoryMax=70%"))

(defvar b/paint-kill-target-on-subprocesses nil)

(defun b/call-process-systemd-kill-target-hook (whole-args)
  (cl-destructuring-bind (program infile destination display &rest args) whole-args
    (when b/paint-kill-target-on-subprocesses
      (setf args (append b/systemd-run-args (cons program args)))
      (setf program "systemd-run"))
    (let ((result (cl-list* program infile destination display args)))
      result)))

(defun b/make-process-systemd-kill-target-hook (args)
  (when (listp (car args))
    (setf args (car args)))
  (when-let* ((_ b/paint-kill-target-on-subprocesses)
              (command (plist-get args :command))
              (exe (car command))
              (new-command
               (append (cl-list* "systemd-run" b/systemd-run-args)
                       (cons (b/find-exe exe) (cdr command)))))
    (plist-put args :command new-command))
  args)

(defun b/compilation-start-paint-kill-target (orig-fun &rest args)
  (let ((b/paint-kill-target-on-subprocesses t))
    (apply orig-fun args)))

(when (eq system-type 'gnu/linux)
  (advice-add 'call-process :filter-args 'b/call-process-systemd-kill-target-hook)
  (advice-add 'make-process :filter-args 'b/make-process-systemd-kill-target-hook)
  (advice-add 'compilation-start :around 'b/compilation-start-paint-kill-target))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(provide 'b-compilation)


