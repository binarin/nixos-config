;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Commentary:
;; Asynchronous niri compositor event stream listener ("niri msg --json event-stream").
;;
;; --- Frame Visibility Tracking ---
;;
;; Visibility is tracked from two event sources:
;;
;; 1. `WindowsOnScreenChanged` — niri reports per-window `visible_percentage`
;;    (0-100) for scroll-driven changes (viewport panning, window moves).
;;
;; 2. `WorkspaceActivated` — workspace switches.  niri does NOT emit
;;    `WindowsOnScreenChanged` during workspace switches because
;;    `on_screen_fraction` is computed per-workspace-viewport which
;;    doesn't change.  Instead we use `WorkspaceActivated` to infer
;;    which windows became visible/invisible.
;;
;; Each Emacs frame encodes its `frame-id` as invisible zero-width
;; characters (ZWNJ/ZWJ) appended to the xdg_toplevel title. This
;; allows reliable identification of Emacs frames from niri's window
;; list, regardless of buffer names or frame-title-format changes.
;;
;; --- Architecture ---
;;
;; 1. `binarin/niri-enable` turns on frame-id encoding, registers event
;;    handlers, and connects to the event stream.
;; 2. On `WindowsChanged` and `WindowOpenedOrChanged`, windows with
;;    app_id="emacs" and our PID are matched to Emacs frames by
;;    decoding the zero-width suffix in the window title.
;; 3. On `WindowsOnScreenChanged`, per-frame visibility is updated and
;;    `binarin/niri-visibility-changed-functions` is invoked.
;; 4. On `WorkspacesChanged`, the full workspace table is synced
;;    (which workspaces exist, which are active, which output each is on).
;; 5. On `WorkspaceActivated`, visibility is recomputed: all frames on
;;    the activated workspace are marked visible; all on the same-output
;;    previously-active workspace are marked invisible.
;; 6. On `WindowClosed`, mappings are cleaned up.
;; 7. A retry timer handles the race between niri's window-open event
;;    and Emacs setting the frame title (via redisplay).

;;; Code:

(defvar binarin/niri--event-process nil
  "The process object for the niri event stream, or nil.")

(defvar binarin/niri--partial-line ""
  "Incomplete line accumulated across filter calls.")

(defvar binarin/niri-event-handlers (make-hash-table :test #'equal)
  "Hash table mapping event type (string) to handler function.
Each handler is called with a single argument: the parsed JSON object
for that event.")

(defun binarin/niri-connect ()
  "Start listening to the niri event stream asynchronously.
Returns the process object, or nil if already connected."
  (interactive)
  (when (and binarin/niri--event-process
             (process-live-p binarin/niri--event-process))
    (user-error "Already connected to niri event stream"))
  (setq binarin/niri--partial-line ""
        binarin/niri--event-process
        (make-process
         :name "b-niri-event-stream"
         :command '("niri" "msg" "--json" "event-stream")
         :connection-type 'pipe
         :noquery t
         :filter #'binarin/niri--event-filter
         :sentinel #'binarin/niri--event-sentinel))
  (message "Connected to niri event stream")
  binarin/niri--event-process)

(defun binarin/niri-disconnect ()
  "Stop the niri event stream process."
  (interactive)
  (when (and binarin/niri--event-process
             (process-live-p binarin/niri--event-process))
    (delete-process binarin/niri--event-process))
  (setq binarin/niri--event-process nil
        binarin/niri--partial-line ""))

(defun binarin/niri--event-filter (_proc string)
  "Process filter for niri event stream.
Accumulates incomplete lines in `binarin/niri--partial-line' and parses
complete JSON lines."
  (let ((data (concat binarin/niri--partial-line string))
        (start 0))
    ;; Process all complete lines
    (while (string-match "\n" data start)
      (let* ((end (match-beginning 0))
             (line (substring data start end)))
        (unless (string-blank-p line)
          (condition-case err
              (binarin/niri--handle-event
               (json-parse-string line :object-type 'alist))
            (error
             (display-warning
              '(niri json)
              (format "Failed to parse niri event: %S\n  Line: %S" err line)))))
        (setq start (1+ end))))
    ;; Keep the remainder for the next filter call
    (setq binarin/niri--partial-line (substring data start))))

(defun binarin/niri--event-sentinel (proc event)
  "Sentinel for the niri event stream process."
  (when (memq (process-status proc) '(exit signal))
    (setq binarin/niri--event-process nil
          binarin/niri--partial-line "")
    (message "Niri event stream disconnected: %s" event)))

(defun binarin/niri--handle-event (json)
  "Dispatch a parsed JSON event to its registered handler.
JSON is an alist representing a single niri event."
  ;; niri events are of the form {"EventName": { ... }}
  (catch 'handled
    (dolist (pair json)
      (let* ((event-type (symbol-name (car pair)))
             (event-data (cdr pair))
             (handler (gethash event-type binarin/niri-event-handlers)))
        (when handler
          (condition-case err
              (funcall handler event-data)
            (error
             (display-warning
              '(niri handler)
              (format "Error in handler for %s: %S" event-type err)))))
        (throw 'handled t)))))

(defun binarin/niri-register-handler (event-type handler)
  "Register HANDLER for EVENT-TYPE niri events.
EVENT-TYPE is a string like \"WorkspacesChanged\" or \"WindowsChanged\".
HANDLER is a function receiving the parsed alist for that event body."
  (puthash event-type handler binarin/niri-event-handlers))

(defun binarin/niri-unregister-handler (event-type)
  "Remove the handler for EVENT-TYPE."
  (remhash event-type binarin/niri-event-handlers))


;;; Window/Frame mapping state

(defvar binarin/niri--our-pid (emacs-pid)
  "Our own PID, used to identify our windows in niri IPC.")

(defvar binarin/niri--windows (make-hash-table :test #'eql)
  "Hash: niri-window-id -> plist (:title :app-id :pid :workspace-id :visible-pct).
Tracks every window that niri knows about which belongs to this Emacs.")

(defvar binarin/niri--workspace-active (make-hash-table :test #'eql)
  "Hash: workspace-id -> t if active (the workspace rendered on its output).")

(defvar binarin/niri--workspace-output (make-hash-table :test #'eql)
  "Hash: workspace-id -> output name string.
Used to determine which other workspaces share an output with a newly
activated workspace, so they can be marked invisible.")

(defvar binarin/niri--window->frame (make-hash-table :test #'eql)
  "Hash: niri-window-id -> Emacs frame.")

(defvar binarin/niri--frame->window (make-hash-table :test #'eq)
  "Hash: Emacs frame -> niri-window-id.")

(defvar binarin/niri-visible-threshold 50
  "Minimum `visible_percentage' for a frame to be considered visible.
0 means any non-zero on-screen area counts as visible.
50 would require at least half the window to be on screen.")

(defvar binarin/niri-visibility-changed-functions nil
  "Abnormal hook called when an Emacs frame's niri visibility changes.
Each function receives two arguments: FRAME and VISIBLE-P (boolean).

Example:
  (add-hook \='binarin/niri-visibility-changed-functions
            (lambda (frame visible-p)
              (if visible-p
                  (set-frame-parameter frame \='alpha 100)
                (set-frame-parameter frame \='alpha 50))))")


;;; Retry machinery for new-window frame matching

(defvar binarin/niri--pending-matches (make-hash-table :test #'eql)
  "Hash: niri-window-id -> (retries-left . timer).
Windows that haven't been matched to an Emacs frame yet.")

(defconst binarin/niri--max-match-retries 20
  "Maximum retry attempts to match a niri window to an Emacs frame.")

(defconst binarin/niri--match-retry-delay 0.1
  "Delay in seconds between matching retries.")


;;; Frame-ID encoding in window title
;;; Uses zero-width characters invisible in all rendering contexts.
;;; ZWNJ (U+200C) = 0, ZWJ (U+200D) = 1.
;;; The frame-id is encoded as a binary string appended to the frame title,
;;; allowing niri (or any compositor) to distinguish Emacs frames by a
;;; unique invisible signature rather than fragile title matching.

(defconst binarin/niri--zw-0 (string #x200C)
  "Zero Width Non-Joiner, represents binary 0 in frame-id encoding.")

(defconst binarin/niri--zw-1 (string #x200D)
  "Zero Width Joiner, represents binary 1 in frame-id encoding.")

(defun binarin/niri--frame-id-encode (frame-id)
  "Encode FRAME-ID as a string of zero-width characters.
Binary digits: 0 -> ZWNJ, 1 -> ZWJ, most significant bit first."
  (let ((num frame-id)
        (bits nil))
    (if (zerop num)
        (setq bits (list ?0))
      (while (> num 0)
        (push (if (zerop (logand num 1)) ?0 ?1) bits)
        (setq num (ash num -1))))
    (apply #'concat
           (mapcar (lambda (c)
                     (if (eq c ?1) binarin/niri--zw-1 binarin/niri--zw-0))
                   bits))))

(defun binarin/niri--frame-id-decode (title)
  "Extract frame-id from TITLE encoded with zero-width characters.
Scans backwards from the end of TITLE, collecting ZWJ/ZWNJ chars
until a non-zero-width character is hit. Returns the decoded
integer, or nil if no encoding found."
  (let ((bits nil)
        (chars (append title nil)))
    (catch 'done
      (dolist (c (reverse chars))
        (cond ((eq c #x200C) (push ?0 bits))
              ((eq c #x200D) (push ?1 bits))
              (t (throw 'done t)))))
    (when bits
      (string-to-number (concat bits) 2))))

(defun binarin/niri--frame-id-suffix ()
  "Return the zero-width encoded frame-id suffix for the selected frame.
Intended for use with (:eval ...) in `frame-title-format'.
During redisplay, `gui_consider_frame_title' selects the target frame
before evaluating the format, so `frame-id' returns the correct value."
  (binarin/niri--frame-id-encode (frame-id)))

(defun binarin/niri--find-frame-by-title (title)
  "Find the Emacs frame whose frame-id matches the encoding in TITLE.
TITLE should be a string from a niri window title.
Returns the frame, or nil if no matching frame is found."
  (let ((id (binarin/niri--frame-id-decode title)))
    (when id
      (cl-find-if (lambda (f) (equal (frame-id f) id))
                  (frame-list)))))

(defun binarin/niri--frame-title-has-suffix-p ()
  "Return non-nil if `frame-title-format' already includes the
frame-id encoding suffix."
  (cl-some (lambda (elt)
             (equal elt '(:eval (binarin/niri--frame-id-suffix))))
           (if (listp frame-title-format)
               frame-title-format
             (list frame-title-format))))

(defun binarin/niri-enable-frame-id-in-title ()
  "Modify `frame-title-format' to append the zero-width encoded frame-id.
Each frame's title will include an invisible binary encoding of its
frame-id, allowing reliable identification by external tools (e.g.,
niri compositor) without affecting the visual title.

Also installs advice on `modify-frame-parameters' to handle frames
with explicit names (which bypass `frame-title-format')."
  (interactive)
  (unless (binarin/niri--frame-title-has-suffix-p)
    ;; Patch frame-title-format for frames without explicit names.
    (setq frame-title-format
          `(,frame-title-format
            (:eval (binarin/niri--frame-id-suffix)))))
  (unless (advice-member-p #'binarin/niri--after-modify-frame-params
                           'modify-frame-parameters)
    (advice-add 'modify-frame-parameters :after
                #'binarin/niri--after-modify-frame-params))
  (message "Frame-id encoding enabled in frame titles"))

(defun binarin/niri--after-modify-frame-params (frame alist)
  "After `modify-frame-parameters', ensure \='title includes frame-id.
When \='name is set (non-nil) and \='title is not also being set, update
\='title to include the zero-width encoded frame-id.  When \='name is
cleared (nil), clear \='title so that `frame-title-format' takes over."
  (let ((name-entry (assq 'name alist))
        (title-entry (assq 'title alist)))
    (when name-entry
      (if (cdr name-entry)
          ;; Explicit name set: inject encoding via 'title if not
          ;; also being set explicitly.
          (unless title-entry
            (set-frame-parameter
             frame 'title
             (concat (cdr name-entry)
                     (binarin/niri--frame-id-encode (frame-id frame)))))
        ;; Name cleared: clear 'title so frame-title-format takes over,
        ;; unless 'title is also explicitly set in the same call.
        (unless title-entry
          (set-frame-parameter frame 'title nil))))))


;;; Window tracking

(defun binarin/niri--app-is-ours-p (app-id pid)
  "Return t if APP-ID and PID belong to this Emacs instance."
  (and (stringp app-id)
       (string= app-id "emacs")
       (integerp pid)
       (= pid binarin/niri--our-pid)))

(defun binarin/niri--cancel-pending-match (window-id)
  "Cancel any pending retry timer for WINDOW-ID."
  (let ((entry (gethash window-id binarin/niri--pending-matches)))
    (when entry
      (cancel-timer (cdr entry))
      (remhash window-id binarin/niri--pending-matches))))

(defun binarin/niri--schedule-match-retry (window-id _title retries-left)
  "Schedule a retry to match WINDOW-ID to an Emacs frame."
  (when (> retries-left 0)
    (let ((timer (run-at-time
                  binarin/niri--match-retry-delay nil
                  (lambda (id)
                    (let ((entry (gethash id binarin/niri--pending-matches)))
                      (when entry
                        (remhash id binarin/niri--pending-matches)
                        (binarin/niri--try-match-frame
                         id (gethash id binarin/niri--windows)))))
                  window-id)))
      (puthash window-id (cons retries-left timer)
               binarin/niri--pending-matches))))

(defun binarin/niri--try-match-frame (window-id rec)
  "Try to match niri WINDOW-ID (with plist REC) to an Emacs frame.
Returns the frame if matched, nil otherwise."
  (let ((title (plist-get rec :title)))
    (when title
      (let ((frame (binarin/niri--find-frame-by-title title)))
        (when frame
          (binarin/niri--map-window-to-frame window-id frame)
          ;; Apply any visibility we already know about
          (let ((pct (plist-get rec :visible-pct)))
            (when pct
              (binarin/niri--update-visibility window-id pct)))
          frame)))))

(defun binarin/niri--track-window (id app-id pid title &optional workspace-id)
  "Record niri window ID and try to match it to an Emacs frame.
WORKSPACE-ID is the niri workspace id this window belongs to."
  (when (binarin/niri--app-is-ours-p app-id pid)
    (let* ((existing (gethash id binarin/niri--windows))
           (rec (list :title title :app-id app-id :pid pid
                      :workspace-id (or workspace-id
                                        (if existing
                                            (plist-get existing :workspace-id)
                                          nil))
                      :visible-pct (if existing
                                       (plist-get existing :visible-pct)
                                     nil))))
      (puthash id rec binarin/niri--windows)
      ;; Try to match
      (unless (binarin/niri--try-match-frame id rec)
        ;; Schedule retries
        (binarin/niri--cancel-pending-match id)
        (binarin/niri--schedule-match-retry id title
                                            binarin/niri--max-match-retries)))))

(defun binarin/niri--untrack-window (id)
  "Remove all state for niri window ID."
  (binarin/niri--cancel-pending-match id)
  (let ((frame (gethash id binarin/niri--window->frame)))
    (when frame
      (binarin/niri--set-frame-visible frame nil)
      (remhash frame binarin/niri--frame->window)))
  (remhash id binarin/niri--window->frame)
  (remhash id binarin/niri--windows))

(defun binarin/niri--map-window-to-frame (window-id frame)
  "Establish bidirectional mapping between niri WINDOW-ID and Emacs FRAME.
Cleans up any previous mappings for either side."
  ;; If this niri window was previously mapped to a different frame
  (let ((old-frame (gethash window-id binarin/niri--window->frame)))
    (when (and old-frame (not (eq old-frame frame)))
      (remhash old-frame binarin/niri--frame->window)
      (binarin/niri--set-frame-visible old-frame nil)))
  ;; If this Emacs frame was previously mapped to a different window
  (let ((old-wid (gethash frame binarin/niri--frame->window)))
    (when (and old-wid (not (eql old-wid window-id)))
      (remhash old-wid binarin/niri--window->frame)))
  ;; Clear any pending retry for this window
  (binarin/niri--cancel-pending-match window-id)
  ;; Establish mapping
  (puthash window-id frame binarin/niri--window->frame)
  (puthash frame window-id binarin/niri--frame->window))


;;; Visibility

(defun binarin/niri--update-visibility (window-id visible-pct)
  "Update visibility state for niri WINDOW-ID based on VISIBLE-PCT.
VISIBLE-PCT is an integer 0-100.  The frame is considered visible
when VISIBLE-PCT >= `binarin/niri-visible-threshold'."
  (let* ((visible-p (> visible-pct binarin/niri-visible-threshold))
         (rec (gethash window-id binarin/niri--windows))
         (frame (gethash window-id binarin/niri--window->frame)))
    ;; Always store the pct even if we don't have a frame yet
    (when rec
      (puthash window-id (plist-put rec :visible-pct visible-pct)
               binarin/niri--windows))
    ;; Notify if we have a mapped frame
    (when frame
      (binarin/niri--set-frame-visible frame visible-p))))

(defun binarin/niri--set-frame-visible (frame visible-p)
  "Set FRAME's visibility state and run hooks if changed."
  (let ((current (frame-parameter frame 'binarin/niri-visible-p)))
    (unless (eq current visible-p)
      (set-frame-parameter frame 'binarin/niri-visible-p visible-p)
      (run-hook-with-args 'binarin/niri-visibility-changed-functions
                          frame visible-p))))

(defun binarin/niri-frame-visible-p (frame)
  "Return t if FRAME is currently visible according to niri.
An unregistered frame returns nil."
  (frame-parameter frame 'binarin/niri-visible-p))


;;; Scan all pending niri windows (called when a new Emacs frame appears)

(defun binarin/niri--scan-pending-for-frame (_frame)
  "Check all unmatched niri windows to see if any match FRAME.
Called from `after-make-frame-functions'."
  (let ((matched nil))
    (maphash
     (lambda (window-id rec)
       (unless (or matched (gethash window-id binarin/niri--window->frame))
         (when (binarin/niri--try-match-frame window-id rec)
           (setq matched t))))
     binarin/niri--windows)))


;;; Event handlers

(defun binarin/niri--on-windows-changed (data)
  "Handle the initial `WindowsChanged' event from niri.
DATA is an alist with key `windows' mapping to a vector of window objects.
Populates the initial window table and matches known Emacs frames."
  (let ((windows (alist-get 'windows data)))
    (when windows
      (cl-loop for w across windows
               for id = (alist-get 'id w)
               for app-id = (alist-get 'app_id w)
               for pid = (alist-get 'pid w)
               for title = (alist-get 'title w)
               for ws-id = (alist-get 'workspace_id w)
               do (binarin/niri--track-window id app-id pid title ws-id))
      (message "b-niri: synced %d windows from niri" (hash-table-count binarin/niri--windows)))))

(defun binarin/niri--on-window-opened-or-changed (data)
  "Handle `WindowOpenedOrChanged' event."
  (let* ((w (alist-get 'window data))
         (id (alist-get 'id w))
         (app-id (alist-get 'app_id w))
         (pid (alist-get 'pid w))
         (title (alist-get 'title w))
         (ws-id (alist-get 'workspace_id w)))
    (binarin/niri--track-window id app-id pid title ws-id)))

(defun binarin/niri--on-window-closed (data)
  "Handle `WindowClosed' event."
  (let ((id (alist-get 'id data)))
    (binarin/niri--untrack-window id)))

(defun binarin/niri--on-windows-on-screen-changed (data)
  "Handle `WindowsOnScreenChanged' event.
DATA has key `changes' -> vector of {window_id, visible_percentage}."
  (let ((changes (alist-get 'changes data)))
    (when changes
      (cl-loop for change across changes
               for id = (alist-get 'window_id change)
               for pct = (alist-get 'visible_percentage change)
               do (binarin/niri--update-visibility id (or pct 0))))))

(defun binarin/niri--on-workspaces-changed (data)
  "Handle `WorkspacesChanged' event.
DATA has key `workspaces' -> vector of workspace objects.
Populates `binarin/niri--workspace-active' and `binarin/niri--workspace-output'."
  (let ((workspaces (alist-get 'workspaces data)))
    (when workspaces
      (clrhash binarin/niri--workspace-active)
      (clrhash binarin/niri--workspace-output)
      (cl-loop for ws across workspaces
               for id = (alist-get 'id ws)
               for output = (alist-get 'output ws)
               for active = (alist-get 'is_active ws)
               do (when active
                    (puthash id t binarin/niri--workspace-active))
               do (when output
                    (puthash id output binarin/niri--workspace-output)))
      ;; After syncing workspace state, recompute frame visibility
      (binarin/niri--apply-workspace-visibility))))

(defun binarin/niri--on-workspace-activated (data)
  "Handle `WorkspaceActivated' event.
DATA has keys `id' (workspace id) and `focused' (bool).
Updates the active-workspace table and recomputes per-frame visibility."
  (let* ((id (alist-get 'id data))
         (output (gethash id binarin/niri--workspace-output)))
    ;; Mark this workspace active.
    (puthash id t binarin/niri--workspace-active)
    ;; Deactivate all other workspaces on the same output.
    (when output
      (let ((to-deactivate nil))
        (maphash (lambda (ws-id _active)
                   (unless (= ws-id id)
                     (when (equal (gethash ws-id binarin/niri--workspace-output) output)
                       (push ws-id to-deactivate))))
                 binarin/niri--workspace-active)
        (dolist (ws-id to-deactivate)
          (remhash ws-id binarin/niri--workspace-active))))
    ;; Recompute visibility for all tracked frames.
    (binarin/niri--apply-workspace-visibility)))

(defun binarin/niri--apply-workspace-visibility ()
  "Recompute per-frame visibility from the active-workspace table.
Each tracked window gets VISIBLE-PCT = 100 if its workspace is active,
0 otherwise.  Does NOT override more precise pct values from
`WindowsOnScreenChanged' for windows on the active workspace — if we
already have a non-zero pct from that event on an active workspace,
we keep it (the coarse 0 vs 100 from the workspace table is only
enough to establish visible/invisible; scroll updates refine it)."
  (maphash
   (lambda (window-id rec)
     (let* ((ws-id (plist-get rec :workspace-id))
            (active-p (and ws-id (gethash ws-id binarin/niri--workspace-active)))
            (new-pct (if active-p
                         ;; Keep existing fine-grained pct if > 0, else assume 100.
                         (let ((cur (plist-get rec :visible-pct)))
                           (if (and cur (> cur 0)) cur 100))
                       0)))
       (binarin/niri--update-visibility window-id new-pct)))
   binarin/niri--windows))


;;; Setup / teardown

(defvar binarin/niri--enabled nil
  "Non-nil when b-niri is fully active (frame-ids + handlers + stream).")

(defun binarin/niri--frame-visible-p-advice (orig-fun frame)
  "Around-advice for `frame-visible-p'.
If b-niri is active, visibility data has arrived, and FRAME is tracked
by niri, return t/nil from niri's perspective.  Otherwise delegate
to ORIG-FUN (the real `frame-visible-p')."
  (if (and binarin/niri--enabled
           (gethash frame binarin/niri--frame->window))
      (if (binarin/niri-frame-visible-p frame) t nil)
    (funcall orig-fun frame)))

(defun binarin/niri-enable ()
  "Enable niri integration: frame-id encoding, event handlers, event stream."
  (interactive)
  ;; 1. Ensure frame-id encoding is active
  (binarin/niri-enable-frame-id-in-title)
  ;; 2. Register event handlers
  (binarin/niri-register-handler "WindowsChanged"
                                 #'binarin/niri--on-windows-changed)
  (binarin/niri-register-handler "WindowOpenedOrChanged"
                                 #'binarin/niri--on-window-opened-or-changed)
  (binarin/niri-register-handler "WindowClosed"
                                 #'binarin/niri--on-window-closed)
  (binarin/niri-register-handler "WindowsOnScreenChanged"
                                 #'binarin/niri--on-windows-on-screen-changed)
  (binarin/niri-register-handler "WorkspacesChanged"
                                 #'binarin/niri--on-workspaces-changed)
  (binarin/niri-register-handler "WorkspaceActivated"
                                 #'binarin/niri--on-workspace-activated)
  ;; 3. Proactive matching: when Emacs creates a new frame, scan pending niri windows
  (add-hook 'after-make-frame-functions #'binarin/niri--scan-pending-for-frame)
  ;; 4. Advise `frame-visible-p' to use niri data when available
  (advice-add 'frame-visible-p :around #'binarin/niri--frame-visible-p-advice)
  ;; 5. Connect to niri event stream
  (binarin/niri-connect)
  (setq binarin/niri--enabled t)
  (message "b-niri: enabled"))

(defun binarin/niri-disable ()
  "Disable niri integration: disconnect, unregister handlers, clean up."
  (interactive)
  (setq binarin/niri--enabled nil)
  ;; Remove advice
  (advice-remove 'frame-visible-p #'binarin/niri--frame-visible-p-advice)
  ;; Disconnect stream
  (binarin/niri-disconnect)
  ;; Unregister handlers
  (binarin/niri-unregister-handler "WindowsChanged")
  (binarin/niri-unregister-handler "WindowOpenedOrChanged")
  (binarin/niri-unregister-handler "WindowClosed")
  (binarin/niri-unregister-handler "WindowsOnScreenChanged")
  (binarin/niri-unregister-handler "WorkspacesChanged")
  (binarin/niri-unregister-handler "WorkspaceActivated")
  ;; Remove frame-creation hook
  (remove-hook 'after-make-frame-functions #'binarin/niri--scan-pending-for-frame)
  ;; Cancel pending retries
  (maphash (lambda (window-id _entry)
             (binarin/niri--cancel-pending-match window-id))
           binarin/niri--pending-matches)
  (clrhash binarin/niri--pending-matches)
  ;; Clear all mappings (reset visibility flags)
  (maphash (lambda (_window-id frame)
             (set-frame-parameter frame 'binarin/niri-visible-p nil))
           binarin/niri--window->frame)
  (clrhash binarin/niri--windows)
  (clrhash binarin/niri--window->frame)
  (clrhash binarin/niri--frame->window)
  (clrhash binarin/niri--workspace-active)
  (clrhash binarin/niri--workspace-output)
  (message "b-niri: disabled"))

(provide 'b-niri)

;;; b-niri.el ends here
