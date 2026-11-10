// last-tab: remembers which tabs were shown, most recent first, so a key can
// go back to the previous one across spaces (tmux's last-window).
//
// It runs as an invisible kept window (TRACKER below: 2x2 px, frameless,
// `keep: true`), which Herdr reopens on every start and whenever its layout
// shows again. tmux-prefix opens it the first time. The history lives in
// shared state (persisted), so a reopened tracker loses nothing.
//
// Readers: tmux-prefix (Ctrl-o Ctrl-o) reads HISTORY itself, since mods can't
// send each other commands. `"last-tab.go"` in keys.json also works, through
// nextCommand(), while the tracker runs.
import { View, div } from "gpui-kit";
import { focused, nextCommand, run, setShared, shared, spaces } from "herdr";

const HISTORY = "last-tab.history"; // tab ids, most recent first
const LIMIT = 30;
const POLL_MS = 500;

export default class LastTab extends View {
  init(_props, cx) {
    // Focus can change without this copy re-rendering; look now and then too.
    // It reads in-memory state and writes only when the tab changed.
    cx.timer.every(POLL_MS, () => {
      // Liveness for tmux-prefix, which reopens the tracker when this is stale.
      globalThis.__herdrLastTabBeat = Date.now();
      this.track();
    });
    cx.spawn(async (cx) => {
      for (;;) {
        if ((await nextCommand()) === "go") this.go();
        cx.notify();
      }
    });
  }

  track() {
    const tab = focused()?.tab;
    if (!tab) return;
    const history = Array.isArray(shared(HISTORY)) ? shared(HISTORY) : [];
    if (history[0] === tab) return;
    setShared(HISTORY, [tab, ...history.filter((id) => id !== tab)].slice(0, LIMIT));
  }

  go() {
    this.track();
    const current = focused()?.tab;
    const live = new Set(spaces().flatMap((s) => s.tabs.map((t) => t.id)));
    const history = Array.isArray(shared(HISTORY)) ? shared(HISTORY) : [];
    const target = history.find((id) => id !== current && live.has(id));
    if (target) run("tab.select", { id: target });
  }

  render() {
    this.track();
    return div().size(0);
  }
}
