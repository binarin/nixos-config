;; -*- lexical-binding: t; -*-
(use-package comint
  :ensure nil
  :defines (comint-input-ring-file-name)
  :commands (comint-read-input-ring comint-write-input-ring))

(defun b/comint-write-history (&optional process event)
  (ignore process event)
  (when comint-input-ring-file-name
    (message "Writing comint history to %s" comint-input-ring-file-name)
    (comint-write-input-ring)))

(defun b/comint-persist-history-setup (filename)
  (when-let* ((process (get-buffer-process (current-buffer))))
    (setf comint-input-ring-file-name (file-name-concat default-directory filename))
    (comint-read-input-ring)
    (set-process-sentinel process #'b/comint-write-history)
    (add-hook 'kill-buffer-hook #'b/comint-write-history nil t)))

(provide 'b-comint)
