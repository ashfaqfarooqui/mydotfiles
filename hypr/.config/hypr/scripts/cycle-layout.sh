#!/bin/bash
current=$(hyprctl getoption general:layout -j | jq -r .str)
if [ "$current" = "dwindle" ]; then
    hyprctl keyword general:layout master
    notify-send "Layout" "Master" -h string:x-canonical-private-synchronous:hypr-layout
else
    hyprctl keyword general:layout dwindle
    notify-send "Layout" "Dwindle" -h string:x-canonical-private-synchronous:hypr-layout
fi
