#!/bin/sh

# default handlers for shellevents
#
# override the function in your own events
# file to provide a custom handler
#
# comments inside each handler list the variables
# that are set when the handler is invoked

STATE=unknown

event_workspace() {
  : # WORKSPACENAME
}

event_workspacev2() {
  : # WORKSPACEID WORKSPACENAME
}

event_focusedmon() {
  : # MONNAME WORKSPACENAME
}

event_activewindow() {
  : # WINDOWCLASS WINDOWTITLE
  if [[ $STATE == emacs && $WINDOWCLASS == emacs ]]; then
      return
  fi

  # XXX multiple unbinds are needed until hyprland 0.46 - https://github.com/hyprwm/Hyprland/commit/936dfedbadbae761cd307ba7509467b55ac5e1ae#diff-a31d81f7206bda96d2e102fae7a3d6f8692e2c0c33fde91e27ae08b8fc067f5fR197
  if [[ $WINDOWCLASS == emacs ]]; then
      STATE=emacs
      hyprctl --batch 'keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash"' > /dev/null
      return
  fi

  mapfile -t keyboards < <(hyprctl devices -j | jq -r '[.keyboards[].name | select(test("keyboard$"))]| sort | .[]')

  TARGET_STATE="${keyboards[*]}"
  if [[ $STATE == $TARGET_STATE ]]; then
      return
  fi

  STATE="$TARGET_STATE"

  hyprctl --batch 'keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash ; keyword unbind CTRL,backslash"' > /dev/null
  for kbd in "${keyboards[@]}"; do
      hyprctl keyword bind "CTRL,backslash,exec,hyprctl switchxkblayout ${kbd} next" > /dev/null
  done
  set +x
}

event_activewindowv2() {
  : # WINDOWADDRESS
}

event_fullscreen() {
  : # ENTER (0 if leaving fullscreen, 1 if entering)
}

event_monitorremoved() {
  : # MONITORNAME
}

event_monitoradded() {
  : # MONITORNAME
}

event_monitoraddedv2() {
  : # MONITORID MONITORNAME MONITORDESCRIPTION
}

event_createworkspace() {
  : # WORKSPACENAME
}

event_createworkspacev2() {
  : # WORKSPACEID WORKSPACENAME
}

event_destroyworkspace() {
  : # WORKSPACENAME
}

event_destroyworkspacev2() {
  : # WORKSPACEID WORKSPACENAME
}

event_moveworkspace() {
  : # WORKSPACENAME MONNAME
}

event_moveworkspacev2() {
  : # WORKSPACEID WORKSPACENAME MONNAME
}

event_renameworkspace() {
  : # WORKSPACEID NEWNAME
}

event_activespecial() {
  : # WORKSPACENAME MONNAME
}

event_activelayout() {
  : # KEYBOARDNAME LAYOUTNAME
  # echo event_activelayout "$KEYBOARDNAME" "$LAYOUTNAME"
}

event_openwindow() {
  : # WINDOWADDRESS WORKSPACENAME WINDOWCLASS WINDOWTITLE
}

event_closewindow() {
  : # WINDOWADDRESS
}

event_movewindow() {
  : # WINDOWADDRESS WORKSPACENAME
}

event_movewindowv2() {
  : # WINDOWADDRESS WORKSPACEID WORKSPACENAME
}

event_windowtitle() {
  : # WINDOWADDRESS
}

event_windowtitlev2() {
  : # WINDOWADDRESS WINDOWTITLE
}

event_openlayer() {
  : # NAMESPACE
}

event_closelayer() {
  : # NAMESPACE
}

event_submap() {
  : # SUBMAPNAME
}

event_changefloatingmode() {
  : # WINDOWADDRESS FLOATING
}

event_urgent() {
  : # WINDOWADDRESS
}

event_minimize() {
  : # WINDOWADDRESS MINIMIZED
}

event_screencast() {
  : # STATE OWNER
}

event_togglegroup() {
  : # STATE WINDOWADDRESSSES
}

event_moveintogroup() {
  : # WINDOWADDRESS
}

event_moveoutofgroup() {
  : # WINDOWADDRESS
}

event_ignoregrouplock() {
  : # STATE
}

event_lockgroups() {
  : # STATE
}

event_configreloaded() {
  :
}

event_pin() {
  : # WINDOWADDRESS PINSTATE
}
