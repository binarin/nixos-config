;; -*- lexical-binding: t; -*-
;;; Altpayment investigation — custom Org link types & dir-locals

(require 'org)
(require 'cl-lib)

;;; pm: link type — pm:Module::Name → Module::Name.org

(defun b/org-pm-open (path _)
  "Open a Perl module org file from a pm: link.
PATH is like \"Module::Name\" or \"Module::Name#search\".
Opens \"Module::Name.org\" relative to the current Org file.
When #SEARCH is present, navigate to search option after opening."
  (let (mod-name search)
    (if (string-match "\\`\\([^#]+\\)#\\(.+\\)\\'" path)
        (setq mod-name (match-string 1 path)
              search (match-string 2 path))
      (setq mod-name path))
    (let ((file (concat mod-name ".org")))
      (find-file file)
      (when search
        (org-link-search search)))))

(org-link-set-parameters "pm"
                         :follow #'b/org-pm-open)

;;; dist: link type — dist://name/module → ./name--module.org

(defun b/org-dist-open (path _)
  "Open a dist org file from a dist: link.
PATH must start with //, like \"//dist/name/module\"
 or \"//dist/name/module::*search\".
Opens \"./dist--name--module.org\" relative to the current Org file.
When ::SEARCH is present, navigate to search option after opening."
  (let (file-part search)
    (if (string-match "\\`\\(.+\\)::\\(.+\\)\\'" path)
        (setq file-part (match-string 1 path)
              search (match-string 2 path))
      (setq file-part path))
    (unless (string-match "\\`//" file-part)
      (user-error "dist: links require // prefix, got: %S" file-part))
    (let* ((cleaned (substring file-part 2))
           (filename (concat "./" (string-join (split-string cleaned "/") "--") ".org")))
      (find-file filename)
      (when search
        (org-link-search search)))))

(org-link-set-parameters "dist"
                         :follow #'b/org-dist-open)

;;; src: link type — src:path → /rpc:adb.k.b:…/path

(defun b/org-src-open (path _)
  "Open a source file from a src: link.
PATH is a file path relative to the repo root, e.g. \"path/to/file\"
 or \"path/to/file::*search\".
Opens \"/rpc:adb.k.b:/usr/local/git_tree/keep/main-altpayment/path/to/file\".
When ::SEARCH is present, navigate to search option after opening."
  (let (file-part search)
    (if (string-match "\\`\\(.+\\)::\\(.+\\)\\'" path)
        (setq file-part (match-string 1 path)
              search (match-string 2 path))
      (setq file-part path))
    (let ((filename (concat "/rpc:adb.k.b:/usr/local/git_tree/keep/main-altpayment/" file-part)))
      (find-file filename)
      (when search
        (org-link-search search)))))

(org-link-set-parameters "src"
                         :follow #'b/org-src-open)

;;; mysql: link type — mysql:Name → ./Name.org

(defun b/org-mysql-open (path _)
  "Open a MySQL org file from a mysql: link.
PATH is like \"Name\" or \"Name::*search\".
Opens \"./Name.org\" relative to the current Org file.
When ::SEARCH is present, navigate to search option after opening."
  (let (file-part search)
    (if (string-match "\\`\\(.+\\)::\\(.+\\)\\'" path)
        (setq file-part (match-string 1 path)
              search (match-string 2 path))
      (setq file-part path))
    (let ((file (concat "./" file-part ".org")))
      (find-file file)
      (when search
        (org-link-search search)))))

(org-link-set-parameters "mysql"
                         :follow #'b/org-mysql-open)

;;; Remote dir-locals for altpayment-investigation

(setq enable-remote-dir-locals t)

(unless (bound-and-true-p byte-compile-current-file)
  (dir-locals-set-class-variables 'altpayment-investigation nil)
  (dir-locals-set-directory-class
   "/rpc:adb.k.b:/usr/local/git_tree/repostat/wip/alt-payment/" 'altpayment-investigation))

(provide 'b-altp)
