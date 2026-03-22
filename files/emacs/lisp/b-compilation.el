;; -*- lexical-binding: t; -*-

(global-set-key (kbd "<f16>") 'recompile)
(global-set-key (kbd "C-<f16>") 'kill-compilation)
(global-set-key (kbd "M-<f16>") 'compile)

(setf compilation-ask-about-save nil)

(provide 'b-compilation)
