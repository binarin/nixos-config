;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Commentary:
;; Asynchronous niri compositor event stream listener.
;; Runs "niri msg --json event-stream" and processes each JSON line
;; using the built-in Emacs JSON parser.
;;
;; --- Frame Visibility Tracking ---
;;
;; Goal: make Emacs aware of which of its frames are currently visible
;; in the niri Wayland compositor.
;;
;; --- Relevant Event Types (from niri-ipc/src/lib.rs) ---
;;
;; WindowsChanged (initial state)
;;   { "windows": [ Window, ... ] }
;;   Sent once on connection. Contains the full current window list.
;;   "Window" objects have fields: id (u64), title (Option<String>),
;;   app_id (Option<String>), pid (Option<i32>), workspace_id (Option<u64>),
;;   is_focused, is_floating, is_urgent, layout, focus_timestamp.
;;
;; WindowOpenedOrChanged
;;   { "window": Window }
;;   A new toplevel opened or an existing one changed (title, workspace, etc.).
;;
;; WindowClosed
;;   { "id": u64 }
;;   A toplevel window was closed.
;;
;; WindowFocusChanged
;;   { "id": u64 | null }
;;   Focus moved to a different window (or none).
;;
;; WindowLayoutsChanged
;;   { "changes": [[id, WindowLayout], ...] }
;;   Window moved between workspaces, resized, etc.
;;   Useful because workspace_id can change when moving a window.
;;
;; WorkspacesChanged (initial state)
;;   { "workspaces": [ Workspace, ... ] }
;;   Full workspace list. "Workspace" has: id, idx, name, output,
;;   is_active, is_focused, is_urgent, active_window_id.
;;   "is_active" means "visible on its output".
;;
;; WorkspaceActivated
;;   { "id": u64, "focused": bool }
;;   A workspace was activated (made visible) or deactivated.
;;   When workspace X becomes active on an output, the previously
;;   active workspace on that output becomes inactive (= not visible).
;;
;; WorkspaceActiveWindowChanged
;;   { "workspace_id": u64, "active_window_id": u64 | null }
;;
;; OverviewOpenedOrClosed
;;   { "is_open": bool }
;;   The overview shows all workspaces at once. When open, windows on
;;   non-active workspaces also become visible.
;;
;; --- Data Available from IPC ---
;;
;; WindowLayout (from niri-ipc/src/lib.rs):
;;   pub struct WindowLayout {
;;       pos_in_scrolling_layout: Option<(usize, usize)>,
;;         -- (column, tile) index, 1-based. Only set for tiled windows.
;;       tile_size: (f64, f64),
;;         -- width and height of the tile (including decorations).
;;       window_size: (i32, i32),
;;         -- actual window geometry, without decorations.
;;       tile_pos_in_workspace_view: Option<(f64, f64)>,
;;         -- position within the workspace view. Only set for floating
;;            windows (src/layout/floating.rs:336). For tiled windows,
;;            this is always None (src/layout/tile.rs:869).
;;       window_offset_in_tile: (f64, f64),
;;         -- window geometry offset within the tile (borders etc).
;;   }
;;
;; NOT available from IPC:
;;   - view_pos (scroll offset) -- ScrollingSpace internal field
;;   - view_size (monitor visible area) -- only #[cfg(test)] getter
;;   - working_area -- not exposed
;;   - column widths and gaps -- not exposed
;;
;; --- Multiple Monitors ---
;;
;; In niri, each monitor independently shows one workspace at a time.
;; Workspace.is_active == true means that workspace is the one currently
;; shown on its output. The focused workspace is the one with keyboard
;; focus (Workspace.is_focused). A window on ANY active workspace (on
;; any monitor) is visible.
;;
;; The WorkspaceActivated event fires when a workspace becomes active
;; on its output. This implicitly deactivates the previously active
;; workspace on that same output.
;;
;; Workspace has fields: id, idx, name, output (output name, or None
;; if no outputs are connected), is_active, is_focused, is_urgent,
;; active_window_id.
;;
;; The "output" field lets us group workspaces by monitor. When a
;; WorkspaceActivated arrives for workspace X (with output="eDP-1"),
;; we know the previously active workspace on "eDP-1" is now hidden.
;;
;; --- Viewports (Scrollable Workspaces) ---
;;
;; In niri, each workspace is a scrollable 2D space. Each monitor shows
;; a viewport (its visible area) over that workspace. The viewport
;; scrolls horizontally as the user navigates between columns.
;;
;; Critical limitation: the IPC event stream does NOT expose:
;;   1. The viewport's scroll offset (view_pos)
;;   2. The viewport's visible size (view_size / working_area)
;;   3. The gaps or column widths needed to reconstruct absolute
;;      positions for tiled windows from their column/tile indices
;;
;; What we DO get per window:
;;   - Floating: tile_pos_in_workspace_view = Some((x, y)) gives the
;;     position in the workspace. Combined with tile_size we have the
;;     full bounding rect in workspace coordinates.
;;   - Tiled: pos_in_scrolling_layout = Some((col, tile)) gives the
;;     logical position in the grid. But without column widths, gaps,
;;     and viewport position, we cannot compute the on-screen rect.
;;
;; --- Strategy for Viewport-Aware Visibility ---
;;
;; Because niri's IPC doesn't expose viewport position, we use a
;; multi-tier approach:
;;
;; Tier 1 (always accurate):
;;   A window whose workspace is NOT active is definitely NOT visible
;;   (its entire workspace is off-screen). This is the only case we
;;   can determine with 100% certainty from the event stream.
;;
;; Tier 2 (approximate, good enough for tiled WM):
;;   A window whose workspace IS active is treated as visible.
;;   Rationale: in a scrollable tiling WM like niri, windows on the
;;   active workspace are virtually always at least partially visible.
;;   The viewport automatically scrolls to keep the focused column
;;   in view, and columns have roughly the same width as the viewport.
;;
;; Tier 3 (exact, for floating windows):
;;   For floating windows specifically, we HAVE tile_pos_in_workspace_view
;;   (position) and tile_size (dimensions). If we also had viewport
;;   info, we could compute exact visibility. Options to get viewport
;;   data:
;;     a) Patch niri to expose view_pos and view_size in the IPC
;;        (/src/layout/scrolling.rs:2294 has view_pos() already public,
;;        but view_size() at line 3717 is #[cfg(test)] only)
;;     b) Estimate viewport position from the focused window's
;;        position (the viewport is scrolled to show the focused column)
;;     c) Periodically query "niri msg windows --json" and cross-
;;        reference tile positions against a heuristically determined
;;        viewport rect
;;
;; Tier 4 (best, for precise 50% threshold):
;;   In the future, we could add a small niri patch to expose:
;;      - Each workspace's current viewport rect (scroll offset + size)
;;      - Maybe a "visible_ratio" field on each window layout
;;   For now, the Tier 2 approximation (active workspace = visible)
;;   is the pragmatic choice for a scrollable tiling WM.
;;
;; Concrete approach:
;;   binarin/niri--visible-threshold :: float, default 0.0 (whole
;;     tile/window must be off-screen to count as invisible). User
;;     can set to 0.5 for "at least 50% visible" requirement.
;;   However, without viewport data, the threshold is only meaningful
;;     for floating windows (where we have position + size). For tiled
;;     windows, any window on the active workspace is assumed visible.
;;
;; --- Multiple Monitors + Viewport Combined ---
;;
;; Each monitor has its own viewport over its active workspace.
;; A window is visible if:
;;   1. Its workspace is the active workspace on SOME monitor, AND
;;   2. The window's tile rect overlaps the viewport of that monitor
;;      by at least the configured threshold
;;
;; Since we lack viewport info, we reduce to condition 1 only.
;;
;; --- Strategy to Map Niri Window IDs to Emacs Frames ---
;;
;; 1. Filter by app_id and pid:
;;    Emacs on Wayland/PGTK sets app_id to "emacs". We also filter
;;    by our own PID (from (emacs-pid)) to avoid matching frames from
;;    other Emacs instances.
;;
;; 2. Match by title:
;;    Niri's Window.title for Emacs frames corresponds to the
;;    xdg_toplevel title, which Emacs derives from the frame's `name`
;;    parameter (set via frame-title-format). Default format:
;;    "%b - GNU Emacs at %U" (buffer-name, hostname).
;;
;;    We compare (frame-parameter f 'name) against the niri title.
;;
;; 3. Mapping table:
;;    binarin/niri--window->frame :: hash niri-window-id -> emacs-frame
;;    binarin/niri--frame->window :: hash emacs-frame -> niri-window-id
;;
;;    On WindowOpenedOrChanged with app_id="emacs" and matching pid,
;;    we look up the Emacs frame whose name matches the niri title.
;;    On WindowClosed, we clean up.
;;
;; 4. Initial bootstrapping:
;;    When connecting, we get WindowsChanged (full snapshot). We iterate
;;    all emacs-app_id windows, match by title against all live Emacs
;;    frames, and populate the mapping.
;;
;; --- Strategy to Determine Frame Visibility ---
;;
;; We maintain:
;;   binarin/niri--workspaces
;;     :: hash workspace-id -> (output-name, is-active, is-focused, ...)
;;   binarin/niri--active-workspaces
;;     :: set of workspace-ids that are active (one per output)
;;   binarin/niri--windows
;;     :: hash niri-window-id -> window data (workspace-id, layout, ...)
;;   binarin/niri--overview-open :: bool
;;   binarin/niri--window->frame  :: inverse of above
;;   binarin/niri--frame->window  :: hash frame -> niri-window-id
;;   binarin/niri--visible-threshold :: float (configurable, default 0.0)
;;
;; Visible region (per monitor) is approximated as the working area
;; of the monitor's active workspace. Without viewport position, we
;; can't compute scroll-dependent overlap. The threshold applies
;; directly only if we augment niri's IPC with viewport data.
;;
;; Visibility check for a frame:
;;   (1) Look up niri-window-id via binarin/niri--frame->window
;;   (2) Look up that window's workspace_id
;;   (3) Check if that workspace_id is active on any output:
;;       (binarin/niri--active-workspaces contains it)
;;       OR if binarin/niri--overview-open is t (overview shows all)
;;   (4) For floating windows: additionally check if the window rect
;;       overlaps the viewport by >= visible-threshold (requires
;;       viewport data from future niri IPC extension)
;;
;; When visibility changes for any emacs frame, we call:
;;   binarin/niri-visibility-changed-functions (abnormal hook, like
;;   window-state-change hooks in Emacs), called with the affected
;;   frame and a boolean (visible-p).
;;
;; --- Edge Cases ---
;;
;; * Title mismatch during frame creation:
;;   When a new Emacs frame is created, the niri WindowOpenedOrChanged
;;   event may arrive before Emacs updates its frame-parameter 'name.
;;   Mitigation: use a short timer to re-attempt matching.
;;
;; * Floating windows on non-active workspaces:
;;   Floating windows are only rendered on the active workspace.
;;   Therefore, workspace-is-active check is sufficient.
;;
;; * Multiple outputs:
;;   Each output has one active workspace. Handled by tracking
;;   per-output active workspace via the output-name field.
;;
;; * Overview:
;;   When overview opens, all workspaces' windows become visible.
;;   When overview closes, only the active workspace's windows are
;;   visible.
;;
;; * WindowLayoutsChanged:
;;   This event includes updated WindowLayout per window. We update
;;   our local window state to reflect new workspace_id and positions.
;;   The event fires when windows move between workspaces, resize,
;;   etc. However, it does NOT fire when the viewport scrolls, so
;;   we cannot detect viewport-scroll-induced visibility changes
;;   without extra IPC data.

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
        (setq num (lsh num -1))))
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
  "After `modify-frame-parameters', ensure 'title includes frame-id.
When 'name is set (non-nil) and 'title is not also being set, update
'title to include the zero-width encoded frame-id.  When 'name is
cleared (nil), clear 'title so that `frame-title-format' takes over."
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

(provide 'b-niri)

;;; b-niri.el ends here
