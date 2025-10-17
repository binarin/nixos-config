(use-package ledger-mode
  :ensure t
  :autoload ledger-context-at-point
  :mode ("\\.hledger\\'" "\\.ledger\\'" "\\.journal\\'"))

(general-setq
 ledger-post-amount-alignment-column 70
 ledger-mode-should-check-version nil
 ledger-report-auto-width nil
 ledger-report-links-in-register nil
 ledger-report-native-highlighting-arguments '("--color=always")
 ledger-default-date-format "%Y-%m-%d"
 ledger-reconcile-default-date-format "%Y-%m-%d")

(defun binarin/csv-dir-for-ledger-account (ledger-account)
  (let ((account (cadr (s-split ":" ledger-account))))
    (file-name-concat (projectile-project-root) "import" account "csv")))

(defun binarin/extract-number (amount)
  (car (s-match "-?[0-9]+\\(?:\\.[0-9]+\\)" amount)))

(defun binarin/global-psv-for-ledger-account (ledger-account)
  (let ((institution (cadr (s-split ":" ledger-account))))
    (file-name-concat (projectile-project-root) "import" institution "rules.psv")))

(cl-defstruct resolve-trn date payee account amount)

(defun binarin/get-resolve-trn ()
  (save-excursion
    (beginning-of-line)
    (when (and (not (looking-at-p ledger-payee-any-status-regex))
               (re-search-backward ledger-payee-any-status-regex nil t))
      (goto-char (match-beginning 0)))
    (let* ((xact-ctx (caddr (ledger-context-at-point)))
           (first-trn-ctx (progn
                            (forward-line)
                            (caddr (ledger-context-at-point)))))
      (make-resolve-trn :date (cadr (assoc 'date xact-ctx))
            :payee (cadr (assoc 'payee xact-ctx))
            :account (cadr (assoc 'account first-trn-ctx))
            :amount (binarin/extract-number (cadr (assoc 'commoditized-amount first-trn-ctx)))))))

;; with date/without date
;; per-file (with date implies)/per-account
;; with amount/without amount
;; with re/without re

(cl-defstruct resolve-scope trn date use-date amount use-amount regex use-regex severity comment rules-file)

(require 'transient)

(defun binarin/resolve-ledger-transaction (scope account)
  (with-slots (date use-date regex use-regex amount use-amount severity comment rules-file) scope
    (let ((date-regex-str "([^,]+)") ;; XXX
          (amnt-regex-str "([^,]+)") ;; XXX
          (desc-regex-str (s-concat ".*" regex ".*"))
          (comment-components '())
          comment-str
          rule-str)
      (when comment (push comment comment-components))
      (push (s-concat "severity:" severity) comment-components)
      (setf comment-str (s-join " " comment-components))
      (setf rule-str (format "%s,%s,%s!%s!%s\n" date-regex-str amnt-regex-str desc-regex-str account comment-str))
      (with-current-buffer (find-file-noselect rules-file)
        (save-excursion
          (goto-char (point-max))
          (insert rule-str)
          (basic-save-buffer))))))

(transient-define-infix binarin/ledger-resolve-regex-infix ()
  "Configure regex")

(defmacro resolve-scope-slot-flipper (slot)
  (let ((cur (gensym)))
    `(let ((,cur (cl-struct-slot-value 'resolve-scope ,slot (transient-scope))))
       (setf (cl-struct-slot-value 'resolve-scope ,slot (transient-scope)) (not ,cur)))))

(transient-define-prefix binarin/ledger-resolve ()
  "Classify unknown transaction."
  ["Filters\n"
   [:description (lambda ()
                   (format "Date: %s"
                           (propertize (resolve-scope-date (transient-scope))
                                       'face (if (resolve-scope-use-date (transient-scope))
                                                 'transient-argument
                                               'transient-inactive-argument))))
                 ("dt" "Toggle use date" (lambda () (interactive) (resolve-scope-slot-flipper 'use-date))
                  :transient t)
                 ("de" "Edit date" (lambda () (interactive) (setf (resolve-scope-date (transient-scope)) (read-from-minibuffer "Date: " (resolve-scope-date (transient-scope)))))
                  :transient t)
                 ""]
   [:description (lambda () (format "RE: %s" (propertize (resolve-scope-regex (transient-scope))
                                                         'face 'transient-argument)))
                 ("re" "Edit regex" (lambda () (interactive) (setf (resolve-scope-regex (transient-scope)) (read-from-minibuffer "Regex: " (resolve-scope-regex (transient-scope)))))
                  :transient t)
                 ""
                 ]]
  ["Actions\n"
   ("g" (lambda () (interactive) (binarin/resolve-ledger-transaction (transient-scope) (binarin/choose-ledger-account))) :description "Resolve to global")]
  (interactive)
  (let ((trn (binarin/get-resolve-trn)))
    (with-slots (date payee amount account) trn
      (transient-setup 'binarin/ledger-resolve nil nil
                       :scope (make-resolve-scope :trn trn
                                                  :date date :use-date nil
                                                  :amount amount :use-amount nil
                                                  :regex (posix-regex-escape payee) :use-regex nil
                                                  :severity "undecided"
                                                  :comment ""
                                                  :rules-file (binarin/global-psv-for-ledger-account account))))))

(defun posix-regex-escape (str) (s-replace-regexp (rx (group (or ?* ?. ?\ ?|))) "\\\\\\1" str))

(defun binarin/choose-ledger-account ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents (file-name-concat (projectile-project-root) "export" "2025-accounts.txt"))
    (completing-read "Account: " (s-split "\n" (buffer-string)))))

(general-define-key :keymaps 'ledger-mode-map
                    "C-c C-r" 'binarin/ledger-resolve)
