;; -*- lexical-binding: t; -*-

(defun b/ripgrep (needle)
  (interactive "sFixed string: ")
  (compilation-start (format (concat
			      "rg --smart-case --fixed-strings --with-filename "
			      "--heading --regexp=%s "
			      "--color always --colors path:none --colors line:none "
			      "--max-columns=300 --max-columns-preview --null ")
			     (shell-quote-argument needle))
		     #'b/ripgrep-mode))



(defun b/ripgrep--heading-filter ()
  )

(define-compilation-mode b/ripgrep-mode "Ripgrep"
  ""
  (setq-local compilation-hidden-output "^rg [^\n]$") ;; XXX
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter nil t)
  (unless kill-transform-function
    (setq-local kill-transform-function #'identity))
  (add-function :filter-return (local 'kill-transform-function)
		(lambda (string)
		  (string-replace "\0" ":" string))))

(provide 'b-ripgrep)
