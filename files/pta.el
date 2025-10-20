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

(cl-defstruct resolve-scope trn date use-date amount use-amount regex use-regex severity comment rules-file project use-project)

(require 'transient)

(defun binarin/resolve-ledger-transaction (scope account)
  (with-slots (date use-date regex use-regex amount use-amount severity comment rules-file project use-project) scope
    (let ((date-regex-str (if use-date date "([^,]+)"))
          (amnt-regex-str "([^,]+)") ;; XXX
          (desc-regex-str (s-concat ".*" regex ".*"))
          (comment-components '())
          comment-str
          rule-str)
      (when comment (push comment comment-components))
      (push (s-concat "severity:" severity) comment-components)
      (when (and use-project project)
        (push (s-concat "project:" project) comment-components))
      (setf comment-str (s-join " " (reverse comment-components)))
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

(defvar binarin/ledger-resolve-comment-history nil)
(defvar binarin/ledger-resolve-last-project nil)
(defun binarin/ledger-resolve-set-comment ()
  (interactive)
  (with-slots (comment) (transient-scope)
    (setf comment (read-from-minibuffer "Comment: " nil nil nil 'binarin/ledger-resolve-comment-history comment))))

(defun binarin/ledger-resolve-set-severity-mandatory ()
  (interactive)
  (binarin/ledger-resolve-set-severity "mandatory"))

(defun binarin/ledger-resolve-set-severity-essential ()
  (interactive)
  (binarin/ledger-resolve-set-severity "essential"))

(defun binarin/ledger-resolve-set-severity-leisure ()
  (interactive)
  (binarin/ledger-resolve-set-severity "leisure"))

(defun binarin/ledger-resolve-set-severity-luxury ()
  (interactive)
  (binarin/ledger-resolve-set-severity "luxury"))

(defun binarin/ledger-resolve-set-severity-burden ()
  (interactive)
  (binarin/ledger-resolve-set-severity "burden"))

(defun binarin/ledger-resolve-set-severity-undecided ()
  (interactive)
  (binarin/ledger-resolve-set-severity "undecided"))

(defun binarin/ledger-resolve-set-severity-unknown ()
  (interactive)
  (binarin/ledger-resolve-set-severity "unknown"))

(defun binarin/ledger-resolve-to-global ()
  (interactive)
  (binarin/resolve-ledger-transaction (transient-scope) (binarin/choose-ledger-account)))


(defun binarin/ledger-resolve-set-severity (value)
  (setf (resolve-scope-severity (transient-scope)) value))

(defmacro binarin/ledger-resolve-severity-setter (bind value)
  `(,bind ,value (lambda () (interactive) (binarin/ledger-resolve-set-severity ,value)) :transient t))


(defun binarin/ledger-resolve-format-date-argument ()
  (format "Date: %s"
          (propertize (resolve-scope-date (transient-scope))
                      'face (if (resolve-scope-use-date (transient-scope))
                                'transient-argument
                              'transient-inactive-argument))))

(defun binarin/ledger-resolve-toggle-use-date ()
  (interactive)
  (resolve-scope-slot-flipper 'use-date))

(defun binarin/ledger-resolve-edit-date ()
  (interactive)
  (setf (resolve-scope-date (transient-scope)) (read-from-minibuffer "Date: " (resolve-scope-date (transient-scope))))
  (setf (resolve-scope-use-date (transient-scope)) t))

(defun binarin/ledger-resolve-format-regex-argument ()
  (format "RE: %s" (propertize (resolve-scope-regex (transient-scope))
                               'face 'transient-argument)))

(defun binarin/ledger-resolve-edit-regex ()
  (interactive)
  (setf (resolve-scope-regex (transient-scope)) (read-from-minibuffer "Regex: " (resolve-scope-regex (transient-scope)))))

(defun binarin/ledger-resolve-format-comment-argument ()
  (format "%s %s"
          (propertize "Comment:" 'face 'transient-heading)
          (aif (resolve-scope-comment (transient-scope))
              (propertize it 'face 'transient-argument)
            (propertize "<NONE>" 'face 'transient-inactive-argument))))

(defun binarin/ledger-resolve-format-project-argument ()
  (format "Project: %s"
          (propertize (or (resolve-scope-project (transient-scope)) "<NONE>")
                      'face (if (resolve-scope-use-project (transient-scope))
                                'transient-argument
                              'transient-inactive-argument))))

(defun binarin/ledger-resolve-toggle-use-project ()
  (interactive)
  (resolve-scope-slot-flipper 'use-project)
  (when (and (resolve-scope-use-project (transient-scope))
             (resolve-scope-project (transient-scope)))
    (let* ((projects-file (file-name-concat (projectile-project-root) "projects.txt"))
           (project-entries (when (file-exists-p projects-file)
                              (with-temp-buffer
                                (insert-file-contents projects-file)
                                (remove "" (s-split "\n" (s-trim (buffer-string)))))))
           (current-project (resolve-scope-project (transient-scope)))
           (project-entry (cl-find-if (lambda (entry)
                                        (string= (car (s-split ":" entry)) current-project))
                                      project-entries))
           (project-severity (when project-entry
                               (cadr (s-split ":" project-entry)))))
      (when project-severity
        (setf (resolve-scope-severity (transient-scope)) project-severity)))))

(defun binarin/ledger-resolve-edit-project ()
  (interactive)
  (let* ((projects-file (file-name-concat (projectile-project-root) "projects.txt"))
         (project-entries (when (file-exists-p projects-file)
                            (with-temp-buffer
                              (insert-file-contents projects-file)
                              (remove "" (s-split "\n" (s-trim (buffer-string)))))))
         (project-names (mapcar (lambda (entry)
                                  (car (s-split ":" entry)))
                                project-entries))
         (current-project (resolve-scope-project (transient-scope)))
         (new-project (completing-read "Project: " project-names nil nil current-project))
         (project-entry (cl-find-if (lambda (entry)
                                      (string= (car (s-split ":" entry)) new-project))
                                    project-entries))
         (project-severity (when project-entry
                             (cadr (s-split ":" project-entry)))))
    (setf (resolve-scope-project (transient-scope)) new-project)
    (setf (resolve-scope-use-project (transient-scope)) t)
    (setf binarin/ledger-resolve-last-project new-project)
    (when project-severity
      (setf (resolve-scope-severity (transient-scope)) project-severity))
    (unless project-entry
      (let ((default-severity (resolve-scope-severity (transient-scope)))
            (new-entry (format "%s:%s" new-project default-severity)))
        (with-temp-buffer
          (when (file-exists-p projects-file)
            (insert-file-contents projects-file))
          (goto-char (point-max))
          (unless (bobp)
            (insert "\n"))
          (insert new-entry)
          (write-file projects-file))))))


(defun binarin/ledger-resolve-format-severity-argument ()
  (format "%s %s"
          (propertize "Severity:" 'face 'transient-heading)
          (propertize (resolve-scope-severity (transient-scope)) 'face 'transient-argument)))

(transient-define-prefix binarin/ledger-resolve ()
  "Classify unknown transaction."
  [:description binarin/ledger-resolve-format-date-argument
                :class transient-row
                ("d" "Edit date" binarin/ledger-resolve-edit-date :transient t)
                ("-d" "Toggle use date" binarin/ledger-resolve-toggle-use-date :transient t)]

  [:description binarin/ledger-resolve-format-regex-argument
                :class transient-row
                ("r" "Edit regex" binarin/ledger-resolve-edit-regex :transient t)]

  [:description binarin/ledger-resolve-format-comment-argument
                :class transient-row
                ("c" "Edit comment" binarin/ledger-resolve-set-comment :transient t)]

  [:description binarin/ledger-resolve-format-project-argument
                :class transient-row
                ("p" "Edit project" binarin/ledger-resolve-edit-project :transient t)
                ("-p" "Toggle use project" binarin/ledger-resolve-toggle-use-project :transient t)]

  [:description binarin/ledger-resolve-format-severity-argument
                :class transient-row
                ("m" "mandatory" binarin/ledger-resolve-set-severity-mandatory :transient t)
                ("t" "essential" binarin/ledger-resolve-set-severity-essential :transient t)
                ("l" "leisure" binarin/ledger-resolve-set-severity-leisure :transient t)
                ("x" "luxury" binarin/ledger-resolve-set-severity-luxury :transient t)
                ("b" "burden" binarin/ledger-resolve-set-severity-burden :transient t)
                ("e" "undecided" binarin/ledger-resolve-set-severity-undecided :transient t)
                ("u" "unknown" binarin/ledger-resolve-set-severity-unknown :transient t)]

  ["Actions"
   ("g" binarin/ledger-resolve-to-global :description "Resolve to global")]

  (interactive)
  (let ((trn (binarin/get-resolve-trn)))
    (with-slots (date payee amount account) trn
      (transient-setup 'binarin/ledger-resolve nil nil
                       :scope (make-resolve-scope :trn trn
                                                  :date date :use-date nil
                                                  :amount amount :use-amount nil
                                                  :regex (hledger-escape-regex payee) :use-regex nil
                                                  :severity "undecided"
                                                  :comment nil
                                                  :rules-file (binarin/global-psv-for-ledger-account account)
                                                  :project binarin/ledger-resolve-last-project :use-project nil)))))

(defun hledger-escape-regex (str)
  (->> str
       (s-replace-regexp (rx (group (any ".+*{}()[]\\"))) "\\\\\\1") ; regex chars
       (s-replace-regexp (rx (any "&!|")) "."))) ; logical operations on hledger matchers + .psv separator

(defun binarin/choose-ledger-account ()
  (interactive)
  (let* ((export-dir (file-name-concat (projectile-project-root) "export"))
         (account-files (directory-files export-dir nil ".*-accounts\\.txt$"))
         (all-accounts '()))
    (dolist (file account-files)
      (with-temp-buffer
        (insert-file-contents (file-name-concat export-dir file))
        (setq all-accounts (append all-accounts (s-split "\n" (buffer-string))))))
    (completing-read "Account: " (delete-dups (remove "" all-accounts)))))

(general-define-key :keymaps 'ledger-mode-map
                    "C-c C-r" 'binarin/ledger-resolve)
