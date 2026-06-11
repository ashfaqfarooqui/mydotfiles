#!/usr/bin/env bash

case "$1" in
    toggle)
        if pgrep -x hypridle > /dev/null; then
            pkill hypridle
        else
            hypridle &
        fi
        # Signal waybar to refresh immediately
        pkill -RTMIN+8 waybar
        ;;
    *)
        if pgrep -x hypridle > /dev/null; then
            echo '{"text": "", "tooltip": "Idle mode active\nClick to inhibit", "class": "active"}'
        else
            echo '{"text": "", "tooltip": "Screen won'\''t sleep\nClick to enable idle", "class": "inhibiting"}'
        fi
        ;;
esac
