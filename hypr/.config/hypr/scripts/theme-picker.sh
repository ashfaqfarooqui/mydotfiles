#!/bin/bash
# Pick a theme and apply it across hypr/waybar/ghostty/rofi/hyprlock (+ wallpaper).

# Hyprland execs this with a minimal PATH, so tools installed to per-user bin
# dirs (e.g. `whiskers` via cargo) aren't found unless we add them here.
export PATH="$HOME/.cargo/bin:$HOME/.local/bin:$PATH"

repo="$HOME/mydotfiles"
theme_dir="$repo/theme"

flavor=$(printf "mocha\nlatte\nfrappe\nmacchiato\nnord\ngruvbox\ndracula\ntokyonight\nrosepine\n" | rofi -dmenu -p "Theme")
[ -z "$flavor" ] && exit 0

just -f "$theme_dir/justfile" -d "$theme_dir" apply "$flavor"
