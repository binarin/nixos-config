;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'recentf)

(ert-deftest config-test-user-emacs-directory-clean ()
  (recentf-save-list)
  (let (unnecessary-files)
    (cl-loop for (file-name) in (directory-files-and-attributes user-emacs-directory)
	     unless (pcase file-name
		      (".dir-locals.el" t)
		      ("init.el" t)
		      ("early-init.el" t)
		      ("tests" t)
		      ("user-lisp" t)
                      ("eln" t)
		      ("." t)
		      (".."t))
	     do (push file-name unnecessary-files))
    (should (equal nil unnecessary-files))))



(require 'b-wprintidle)
(require 'l-windows)
(require 'b-startup)

;; Defined in niri-frame-visible.el (emacs-niri-awareness), which is
;; not on the test load-path.  Provide a default so consumer code that
;; binds it can be exercised.
(defvar niri-frame-visible-inhibit nil)

;; Shared harness for the window-cycle tests.  Windows and frames are
;; plain symbols; the window primitives are stubbed to read from an
;; explicit window->frame map and per-frame window ordering, so the
;; cycle logic can be exercised without real frames.
(defmacro b/with-fake-windows (spec &rest body)
  "Run BODY with window primitives stubbed from SPEC.
SPEC is (:frames FRAMES :windows ((FRAME . WINDOWS)...)
         :selected-frame SF :selected-window SW
         &optional :visible-p FN :no-other WINDOWS).
FRAMES is the `frame-list' order.  :visible-p defaults to all-visible.
:no-other lists windows for which `window-no-other-p' returns t."
  (declare (indent 1))
  `(let* ((spec (list ,@spec))
          (frames (plist-get spec :frames))
          (fwins (plist-get spec :windows))
          (sf (plist-get spec :selected-frame))
          (sw (plist-get spec :selected-window))
          (visible-p (or (plist-get spec :visible-p) (lambda (_f) t)))
          (no-other (plist-get spec :no-other))
          (win->frame (cl-loop for (f . ws) in fwins
                               append (mapcar (lambda (w) (cons w f)) ws))))
     (cl-letf (((symbol-function 'frame-list) (lambda () frames))
               ((symbol-function 'selected-frame) (lambda () sf))
               ((symbol-function 'selected-window) (lambda () sw))
               ((symbol-function 'frame-visible-p) visible-p)
               ((symbol-function 'window-frame)
                (lambda (w) (cdr (assq w win->frame))))
               ((symbol-function 'frame-first-window)
                (lambda (f) (car (cdr (assq f fwins)))))
               ((symbol-function 'window-no-other-p)
                (lambda (w) (and (memq w no-other) t)))
               ((symbol-function 'window-list)
                (lambda (&optional f _mini w)
                  (let ((ws (cdr (assq f fwins))))
                    (if w (append (memq w ws)
                                  (cl-subseq ws 0 (cl-position w ws)))
                      ws)))))
       ,@body)))

(defmacro b/with-fake-frames (spec &rest body)
  "Run BODY with frame primitives stubbed from SPEC.
SPEC is (:frames FRAMES :selected SF :closing CLOSE
         :visible-p FN :parent PARENT-ALIST :delete-before DB-ALIST).
:closing is the frame being closed.  :visible-p defaults to all-visible
and is what `frame-visible-p' (advised or not) will report.
:parent / :delete-before are alists frame->value for frame-parent /
the delete-before parameter."
  (declare (indent 1))
  `(let* ((spec (list ,@spec))
          (frames (plist-get spec :frames))
          (sf (plist-get spec :selected-frame))
          (visible-p (or (plist-get spec :visible-p) (lambda (_f) t)))
          (parents (plist-get spec :parents))
          (delete-before (plist-get spec :delete-before)))
     (cl-letf (((symbol-function 'frame-list) (lambda () frames))
               ((symbol-function 'selected-frame) (lambda () sf))
               ((symbol-function 'frame-visible-p) visible-p)
               ((symbol-function 'frame-parent)
                (lambda (f) (cdr (assq f parents))))
               ((symbol-function 'frame-parameter)
                (lambda (f prop)
                  (when (and (eq prop 'delete-before)
                             (assq f delete-before))
                    (cdr (assq f delete-before))))))
       ,@body)))

(ert-deftest b/handle-delete-frame-deletes-when-siblings-on-other-workspaces ()
  "Closing the last frame on the active niri workspace must
`delete-frame' — not escalate to `save-buffers-kill-emacs' — when
other frames exist on inactive workspaces.

Regression: `track-niri-frame-visibility-mode' advises
`frame-visible-p' to return nil for frames on inactive workspaces,
which made `handle-delete-frame' think no other frame existed and
route the close to `save-buffers-kill-emacs', where `emacs-lock'
then vetoed the quit — stranding the frame."
  ;; Model the two-layer structure: an underlying `frame-visible-p'
  ;; that truthfully reports every frame visible, plus a fake niri
  ;; override layered on top that hides frames not on the active
  ;; workspace.  `b/handle-delete-frame-1' must (by binding
  ;; `niri-frame-visible-inhibit') bypass that override and see the
  ;; siblings, routing to `delete-frame'.
  (let (route
        (override (lambda (orig f)
                    (if niri-frame-visible-inhibit
                        (funcall orig f)
                      (eq f 'ws-a)))))
    (cl-letf* (((symbol-function 'frame-list) (lambda () '(ws-a ws-b ws-c)))
              ((symbol-function 'selected-frame) (lambda () 'ws-a))
              ((symbol-function 'frame-parent) (lambda (_f) nil))
              ((symbol-function 'frame-parameter) (lambda (_f _prop) nil))
              ;; Underlying `frame-visible-p': all frames visible.
              ((symbol-function 'frame-visible-p) (lambda (_f) t))
              ((symbol-function 'delete-frame)
               (lambda (&rest _) (setq route 'delete-frame)))
              ((symbol-function 'save-buffers-kill-emacs)
               (lambda (&optional _arg)
                 (setq route 'save-buffers-kill-emacs))))
      ;; niri override: only the active-workspace frame is visible.
      (advice-add 'frame-visible-p :around override)
      (unwind-protect
          (b/handle-delete-frame-1 'ws-a)
        (advice-remove 'frame-visible-p override)))
    (should (eq route 'delete-frame))))

(ert-deftest b/handle-delete-frame-quits-when-truly-last-frame ()
  "Closing the globally-last frame still routes to
`save-buffers-kill-emacs' (pgtk Emacs cannot run frameless, so the
last close must quit)."
  (let (route)
    (b/with-fake-frames (:frames '(sole)
                         :selected-frame 'sole
                         :visible-p (lambda (_f) t))
      (cl-letf (((symbol-function 'delete-frame)
                 (lambda (&rest _) (setq route 'delete-frame)))
                ((symbol-function 'save-buffers-kill-emacs)
                 (lambda (&optional _arg)
                   (setq route 'save-buffers-kill-emacs))))
        (b/handle-delete-frame-1 'sole)))
    (should (eq route 'save-buffers-kill-emacs))))

(ert-deftest b-org-test-wprintidle-socket-path-xdg ()
  "Socket path uses $XDG_RUNTIME_DIR when set."
  (let ((process-environment (cons "XDG_RUNTIME_DIR=/run/user/12345"
                                   process-environment)))
    (should (equal "/run/user/12345/wprintidle-c.sock"
                   (b/wprintidle-socket-path)))))

(ert-deftest b-org-test-wprintidle-socket-path-tmp-fallback ()
  "Socket path falls back to /tmp/wprintidle-c-<uid>.sock without XDG_RUNTIME_DIR."
  (let ((process-environment
         (cl-remove-if (lambda (e) (string-prefix-p "XDG_RUNTIME_DIR=" e))
                       process-environment)))
    (should (equal (format "/tmp/wprintidle-c-%d.sock" (user-uid))
                   (b/wprintidle-socket-path)))))

(ert-deftest b-org-test-parse-wprintidle-response-seconds ()
  "A numeric line parses to a float."
  (should (equal 42.0 (b/parse-wprintidle-response "42\n"))))

(ert-deftest b-org-test-parse-wprintidle-response-active ()
  "\"0\\n\" means active user -> 0.0, not nil."
  (should (equal 0.0 (b/parse-wprintidle-response "0\n"))))

(ert-deftest b-org-test-parse-wprintidle-response-garbage ()
  "Non-numeric / empty input returns nil."
  (should (equal nil (b/parse-wprintidle-response "")))
  (should (equal nil (b/parse-wprintidle-response "garbage\n"))))
