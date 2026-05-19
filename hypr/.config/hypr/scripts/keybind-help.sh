#!/bin/bash

decode_modmask() {
    local mask=$1 parts=()
    (( mask & 64 )) && parts+=("SUPER")
    (( mask & 1 ))  && parts+=("SHIFT")
    (( mask & 4 ))  && parts+=("CTRL")
    (( mask & 8 ))  && parts+=("ALT")
    local IFS="+"
    echo "${parts[*]}"
}

format_key() {
    local key=$1
    case "$key" in
        return)        echo "Return" ;;
        escape)        echo "Esc" ;;
        space)         echo "Space" ;;
        tab)           echo "Tab" ;;
        comma)         echo "," ;;
        period)        echo "." ;;
        slash)         echo "/" ;;
        mouse_down)    echo "ScrollDown" ;;
        mouse_up)      echo "ScrollUp" ;;
        *)             echo "$key" ;;
    esac
}

build_cheatsheet() {
    hyprctl binds -j | jq -r '
        map(select(.has_description and (.description != ""))) |
        group_by(.submap) |
        sort_by(if .[0].submap == "" then 0 else 1 end) |
        .[] |
        (if .[0].submap == "" then "GLOBAL" else .[0].submap end) as $sec |
        .[] |
        "\($sec)\t\(.modmask)\t\(.key)\t\(.description)"
    ' | while IFS=$'\t' read -r section modmask key desc; do
        mods=$(decode_modmask "$modmask")
        k=$(format_key "$key")
        if [ -n "$mods" ]; then
            printf "%-22s  %s\n" "${mods} + ${k}" "$desc"
        else
            printf "%-22s  %s\n" "$k" "$desc"
        fi
    done
}

build_cheatsheet | rofi -dmenu -i -p "" -theme ~/.config/hypr/scripts/cheatsheet.rasi -no-custom -markup-rows
