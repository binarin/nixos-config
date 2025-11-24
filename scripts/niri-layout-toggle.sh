#!/usr/bin/env bash
set -euo pipefail

# Get the app_id of the focused window
app_id=$(niri msg -j focused-window | jq -r '.app_id')

# If it's emacs, use wtype to send Ctrl+\, otherwise toggle layout
if [[ "$app_id" == "emacs" ]]; then
    wtype -M ctrl -M shift -M alt -M win '|'
else
    niri msg action switch-layout next
fi
