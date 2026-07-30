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
		      ("." t)
		      (".."t))
	     do (push file-name unnecessary-files))
    (should (equal nil unnecessary-files))))



(require 'b-wprintidle)
(require 'l-windows)

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

(ert-deftest b/window-cycle-is-stable-regardless-of-selection ()
  "b/window-cycle returns a fixed ring in frame-list / natural window
order, independent of which window is selected."
  ;; Same frames/windows, different selected-window: identical cycle.
  (b/with-fake-windows (:frames '(f1 f2)
                        :windows '((f1 a b) (f2 c d))
                        :selected-frame 'f1 :selected-window 'a)
    (should (equal (b/window-cycle) '(a b c d))))
  (b/with-fake-windows (:frames '(f1 f2)
                        :windows '((f1 a b) (f2 c d))
                        :selected-frame 'f1 :selected-window 'b)
    (should (equal (b/window-cycle) '(a b c d)))))

(ert-deftest b/window-cycle-excludes-invisible-frames ()
  "Frames failing frame-visible-p are excluded from the cycle,
but the selected frame is always kept."
  (b/with-fake-windows (:frames '(f1 f2 f3)
                        :windows '((f1 a) (f2 b) (f3 c))
                        :selected-frame 'f1 :selected-window 'a
                        :visible-p (lambda (f) (not (eq f 'f2))))
    ;; f2 hidden -> dropped; f1 (selected) and f3 remain.
    (should (equal (b/window-cycle) '(a c)))))

(ert-deftest b/window-cycle-drops-no-other-windows ()
  "Windows for which window-no-other-p is non-nil are dropped."
  (b/with-fake-windows (:frames '(f1 f2)
                        :windows '((f1 a) (f2 b c))
                        :selected-frame 'f1 :selected-window 'a
                        :no-other '(b))
    ;; b is no-other -> dropped; a and c remain.
    (should (equal (b/window-cycle) '(a c)))))

(ert-deftest b/other-window-selects-crossing-frame-window ()
  "b/other-window 1 selects the next window in the cycle, crossing to
another visible frame, and does not FocusWindow when disconnected."
  (let (selected focus-calls)
    (b/with-fake-windows (:frames '(f1 f2)
                          :windows '((f1 a) (f2 b))
                          :selected-frame 'f1 :selected-window 'a)
      (cl-letf (((symbol-function 'select-window)
                 (lambda (w &rest _) (setq selected w)))
                ((symbol-function 'niri-rpc-connected-p) (lambda () nil))
                ((symbol-function 'niri-rpc-focus-window)
                 (lambda (id) (push id focus-calls))))
        (b/other-window 1)))
    (should (eq selected 'b))
    (should (null focus-calls))))

(ert-deftest b/other-window-crosses-from-multi-window-frame ()
  "From the last window of a multi-window frame, b/other-window 1
advances to the next frame instead of looping back inside the frame.
Regression: a rotated cycle trapped navigation inside the frame."
  (let (selected)
    (b/with-fake-windows (:frames '(f1 f2)
                          :windows '((f1 a b) (f2 c))
                          ;; selected is the SECOND window of f1.
                          :selected-frame 'f1 :selected-window 'b)
      (cl-letf (((symbol-function 'select-window)
                 (lambda (w &rest _) (setq selected w)))
                ((symbol-function 'niri-rpc-connected-p) (lambda () nil)))
        (b/other-window 1)))
    ;; cycle (a b c), start b (idx 1), +1 -> c on the other frame.
    (should (eq selected 'c))))

(ert-deftest b/other-window-negative-count-wraps-backward ()
  "b/other-window -1 moves backward through the cycle (mod length)."
  (let (selected)
    (b/with-fake-windows (:frames '(f1 f2)
                          :windows '((f1 a) (f2 b))
                          :selected-frame 'f1 :selected-window 'a)
      (cl-letf (((symbol-function 'select-window)
                 (lambda (w &rest _) (setq selected w)))
                ((symbol-function 'niri-rpc-connected-p) (lambda () nil)))
        (b/other-window -1)))
    ;; cycle (a b), (mod -1 2) = 1 -> b.
    (should (eq selected 'b))))

(ert-deftest b/other-window-no-select-when-target-is-start ()
  "When COUNT wraps back to the starting window, nothing is selected
and no FocusWindow is sent."
  (let (selected focus-calls)
    (b/with-fake-windows (:frames '(f1 f2)
                          :windows '((f1 a) (f2 b))
                          :selected-frame 'f1 :selected-window 'a)
      (cl-letf (((symbol-function 'select-window)
                 (lambda (w &rest _) (setq selected w)))
                ((symbol-function 'niri-rpc-connected-p) (lambda () t))
                ((symbol-function 'niri-rpc-focus-window)
                 (lambda (id) (push id focus-calls))))
        ;; cycle length 2, count 2 -> (mod 2 2) = 0 -> start (a).
        (b/other-window 2)))
    (should (null selected))
    (should (null focus-calls))))

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
