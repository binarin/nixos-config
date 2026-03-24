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

(defun b/start-process-systemd-kill-target-hook (orig-fun command infile destination display &rest args)
  (when b/paint-kill-target-on-subprocesses
    (setf command "systemd-run"
	  args (append b/systemd-run-args args)))
  (apply orig-fun command infile destination display args))

(cl-defun b/make-process-systemd-kill-target-hook (orig-fun &rest args &key command &allow-other-keys)
  (when (and command
	     b/paint-kill-target-on-subprocesses)
    (let ((exe (car command))
	  (cmd-args (cdr command)))
      (plist-put args :command (append
				(cl-list* "systemd-run" b/systemd-run-args)
				(list (b/find-exe exe)) cmd-args))))
  (apply orig-fun args))

(defun b/compilation-start-paint-kill-target (orig-fun &rest args)
  (let ((b/paint-kill-target-on-subprocesses t))
    (apply orig-fun args)))

(when (eq system-type 'gnu/linux)
  (advice-add 'call-process :around #'b/start-process-systemd-kill-target-hook)
  (advice-add 'make-process :around #'b/make-process-systemd-kill-target-hook)
  (advice-add 'compilation-start :around #'b/compilation-start-paint-kill-target))

(provide 'b-compilation)


