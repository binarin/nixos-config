// tmux-prefix, adapted from herdrdev/herdr-layout-examples (Apache-2.0),
// tmux-prefix/mods/tmux-prefix at ed4eda7a02e255191203df0ff04b9dc7ae56e1ad:
// https://github.com/herdrdev/herdr-layout-examples/tree/main/tmux-prefix
//
// Changes from upstream, to match modules/binarin/tmux.nix (prefix C-o,
// pain-control, base-index 1):
// - The prefix is Ctrl+O. Ctrl+O Ctrl+O goes to the last tab (tmux
//   last-window, through the last-tab mod's history); Ctrl+O o sends a
//   literal ^O (tmux send-prefix).
// - Bindings may carry Ctrl (pain-control's C-h/j/k/l).
// - More tmux keys: | \ - _ splits, & close tab, , $ rename, ( ) spaces,
//   : palette, s picker, b sidebar.
// - Opens the last-tab tracker when it isn't running.
import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { closeOverlay, focused, hc, nextCommand, openOverlay, props, run, shared, spaces, theme } from "herdr";

// Only repeatable actions start a timer: the first key after the prefix can
// take as long as you like.
const REPEAT_MS = 500;
// [key, label, command, args, repeatable]. Keys are what a plain press types
// (key_char, so "%", "J", "|"), or "ctrl-<key>" with Ctrl held.
const BINDINGS = [
  ["ctrl-o", "Last tab", "last-tab"],
  ["c", "New tab", "tab.new"],
  ["n", "Next tab", "tab.next"],
  ["p", "Previous tab", "tab.prev"],
  [",", "Rename tab", "tab.rename"],
  ["&", "Close tab", "tab.close"],
  ["|", "Split right", "pane.split", { direction: "right" }],
  ["\\", "Split right", "pane.split", { direction: "right" }],
  ["%", "Split right", "pane.split", { direction: "right" }],
  ["-", "Split down", "pane.split", { direction: "down" }],
  ["_", "Split down", "pane.split", { direction: "down" }],
  ['"', "Split down", "pane.split", { direction: "down" }],
  ["x", "Close pane", "pane.close"],
  ["z", "Zoom pane", "pane.zoom"],
  ["h", "Focus left", "pane.go", { direction: "left" }, true],
  ["j", "Focus down", "pane.go", { direction: "down" }, true],
  ["k", "Focus up", "pane.go", { direction: "up" }, true],
  ["l", "Focus right", "pane.go", { direction: "right" }, true],
  ["ctrl-h", "Focus left", "pane.go", { direction: "left" }, true],
  ["ctrl-j", "Focus down", "pane.go", { direction: "down" }, true],
  ["ctrl-k", "Focus up", "pane.go", { direction: "up" }, true],
  ["ctrl-l", "Focus right", "pane.go", { direction: "right" }, true],
  ["left", "Focus left", "pane.go", { direction: "left" }, true],
  ["down", "Focus down", "pane.go", { direction: "down" }, true],
  ["up", "Focus up", "pane.go", { direction: "up" }, true],
  ["right", "Focus right", "pane.go", { direction: "right" }, true],
  ["J", "Next space", "space.next", {}, true],
  ["K", "Previous space", "space.prev", {}, true],
  [")", "Next space", "space.next"],
  ["(", "Previous space", "space.prev"],
  ["$", "Rename space", "space.rename"],
  ["s", "Go to… (picker)", "mod.open", { name: "picker" }],
  [":", "Command palette", "app.palette"],
  ["b", "Toggle sidebar", "layout.toggle", { id: "sidebar" }],
  ["d", "Minimize window", "window.minimize"],
  ...Array.from({ length: 10 }, (_, i) => [String(i), `Tab ${i === 0 ? 10 : i}`, "tab.select", { index: i === 0 ? 10 : i }]),
];
// Shown only in the full list (`?`).
const DETAIL = (key) => /^(left|right|up|down|ctrl-[hjkl]|\d)$/.test(key) || ["\\", "%", "_", '"'].includes(key);

// The last-tab mod's tracker and history (see mods/last-tab/main.js).
const LAST_TAB_HISTORY = "last-tab.history";
const LAST_TAB_TRACKER = {
  mod: "last-tab", kind: "window", key: "last-tab-tracker", keep: true, frame: "none",
  width: 2, height: 2, at: { x: 0, y: 0 }, props: { role: "tracker" },
};

function ensureLastTabTracker() {
  // The tracker beats every 500 ms while it runs; reopening replaces it in place.
  if (Date.now() - (globalThis.__herdrLastTabBeat ?? 0) > 2000) openOverlay(LAST_TAB_TRACKER);
}

function lastTab() {
  const current = focused()?.tab;
  const live = new Set(spaces().flatMap((s) => s.tabs.map((t) => t.id)));
  const history = Array.isArray(shared(LAST_TAB_HISTORY)) ? shared(LAST_TAB_HISTORY) : [];
  const target = history.find((id) => id !== current && live.has(id));
  if (target) run("tab.select", { id: target });
}

function forwardKey(e, key) {
  if (["enter", "tab", "backspace", "left", "right", "up", "down", "space"].includes(e.key)) return e.key;
  if (key === " ") return "space";
  if (/^[A-Z]$/.test(key)) return `shift+${key.toLowerCase()}`;
  if (key.length === 1 && "abcdefghijklmnopqrstuvwxyz0123456789`\\[],=-.';/".includes(key)) return key === "\\" ? "backslash" : key;
  return null;
}

export default class TmuxPrefix extends View {
  init(_props, cx) {
    this.handle = cx.focus_handle();
    this.handle.focus();
    this.repeat = false;
    this.generation = 0;
    this.all = false;
    ensureLastTabTracker();
    // keys.json's mod:tmux-prefix section beats the global opener while we're
    // open, so a second Ctrl+O arrives here as "last-tab".
    cx.spawn(async (cx) => {
      for (;;) {
        const command = await nextCommand();
        if (command === "send-prefix") this.sendPrefix(cx);
        else if (command === "last-tab") this.act(BINDINGS[0], cx);
      }
    });
  }

  dismiss() {
    closeOverlay(props().overlay);
  }

  send(key, cx) {
    this.generation++;
    const terminal = focused()?.terminal;
    if (!terminal) return this.dismiss();
    // hc starts its subprocess at the call, before closing cancels this view's tasks.
    const pending = hc(["keys", terminal, key]);
    cx.spawn(async () => {
      const result = await pending;
      if (result.code !== 0) console.log(`tmux-prefix: ${result.stderr}`);
    });
    this.dismiss();
  }

  sendPrefix(cx) {
    this.send("ctrl+o", cx);
  }

  act(binding, cx) {
    const [, , command, args = {}, repeatable = false] = binding;
    if (!repeatable) this.dismiss();
    if (command === "last-tab") lastTab();
    else run(command, args);
    if (repeatable) {
      this.repeat = true;
      const generation = ++this.generation;
      cx.timer.after(REPEAT_MS, () => {
        if (this.generation === generation) this.dismiss();
      });
      cx.notify();
    }
  }

  key(e, cx) {
    cx.stop_propagation();
    const m = e.modifiers;
    if (e.key === "escape") return this.dismiss();
    const plain = !m.control && !m.alt && !m.platform;
    const ctrl = m.control && !m.alt && !m.platform;
    const key = plain ? (e.key_char ?? (m.shift && e.key.length === 1 ? e.key.toUpperCase() : e.key)) : e.key;
    if (plain && key === "o") return this.sendPrefix(cx);
    if (plain && key === "?") {
      this.all = !this.all;
      this.repeat = false;
      this.generation++;
      cx.notify();
      return;
    }
    const name = plain ? key : ctrl ? `ctrl-${e.key}` : null;
    const binding = name && BINDINGS.find((b) => b[0] === name);
    if (!binding) {
      const forwarded = this.repeat && plain && forwardKey(e, key);
      if (forwarded) return this.send(forwarded, cx);
      return this.dismiss();
    }
    this.act(binding, cx);
  }

  render() {
    const t = theme();
    const rows = this.all ? BINDINGS : BINDINGS.filter(([key]) => !DETAIL(key));
    return v_flex().id("tmux-prefix").track_focus(this.handle)
      .on_key_down((e, cx) => this.key(e, cx))
      .p(14).gap(8).bg(t.popover).text_color(t.foreground).text_size(12)
      .child(h_flex().justify_between().child(div().font_semibold().child(this.repeat ? `Repeat · ${REPEAT_MS} ms` : "Ctrl+O · waiting for a key"))
        .child(div().text_color(t.muted).child("Esc cancel · o send ^O")))
      .child(v_flex().h(280).min_h(0).overflow_y_scrollbar().children(rows.map(([key, label, , , repeatable]) =>
        h_flex().h(20).flex_none().items_center().gap(12).child(div().w(48).text_color(t.accent).child(key))
          .child(div().flex_1().child(label)).child(div().text_color(t.dim).child(repeatable ? "repeat" : "")))))
      .child(div().text_color(t.muted).child("1–9 tabs (0 = 10) · ^O^O last tab · ? all bindings"));
  }
}
