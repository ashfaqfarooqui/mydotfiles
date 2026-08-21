#!/bin/bash
# Pick a theme and apply it across hypr/waybar/ghostty/rofi/hyprlock (+ wallpaper).
# Shows a color-swatch icon and proper display name per theme, like Omarchy's
# theme selector, and marks whichever one is currently active.

# Hyprland execs this with a minimal PATH, so tools installed to per-user bin
# dirs (e.g. `whiskers` via cargo) aren't found unless we add them here.
export PATH="$HOME/.cargo/bin:$HOME/.local/bin:$PATH"

repo="$HOME/mydotfiles"
theme_dir="$repo/theme"
current=$(cat "$theme_dir/.current" 2>/dev/null || echo "")

declare -A display_names=(
    [mocha]="Catppuccin Mocha"
    [latte]="Catppuccin Latte"
    [frappe]="Catppuccin Frappé"
    [macchiato]="Catppuccin Macchiato"
    [nord]="Nord"
    [gruvbox]="Gruvbox"
    [dracula]="Dracula"
    [tokyonight]="Tokyo Night"
    [rosepine]="Rosé Pine"
)
order=(mocha latte frappe macchiato nord gruvbox dracula tokyonight rosepine)

menu=""
declare -A slug_for_label
for name in "${order[@]}"; do
    label="${display_names[$name]}"
    [ "$name" = "$current" ] && label="${label} (current)"
    slug_for_label["$label"]="$name"
    menu+="${label}\0icon\x1f${theme_dir}/swatches/${name}.png\n"
done

selected=$(printf "%b" "$menu" | rofi -dmenu -show-icons -p "Theme")
[ -z "$selected" ] && exit 0

flavor="${slug_for_label[$selected]}"
[ -z "$flavor" ] && exit 0

just -f "$theme_dir/justfile" -d "$theme_dir" apply "$flavor"
