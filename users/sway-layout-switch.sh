#!/usr/bin/env bash
[[ $1 == "doit" ]] && {
    current=$(swaymsg -t get_inputs | jq '[.[] | select(.type=="keyboard")] | .[0].xkb_active_layout_index')
    if [[ $current == 0 ]]; then
        current=1
    else
        current=0
    fi

    swaymsg "input * xkb_switch_layout $current"
    exit 0
}

swaymsg -m -t subscribe '["window"]' |
jq --unbuffered -r 'select(.change == "focus") | .container.app_id' |
while read new_app; do
    if [[ "$new_app" == "emacs" ]]; then
        swaymsg -- "unbindsym Ctrl+Backslash"
    else
        swaymsg -- "bindsym Ctrl+Backslash exec $0 doit"
    fi
done
