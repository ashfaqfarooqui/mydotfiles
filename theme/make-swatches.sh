#!/usr/bin/env bash
# Build a small 2x2 color-swatch PNG per theme (base/accent/blue/red) from its
# generated hypr-theme.lua, for use as rofi row icons in the theme picker.
set -euo pipefail
cd "$(dirname "$0")"

hex() {
    grep -m1 "^M\.$2 = " "generated/$1/hypr-theme.lua" | sed -E 's/.*rgb\(([0-9a-f]{6})\).*/#\1/'
}

for name in mocha latte frappe macchiato nord gruvbox dracula tokyonight rosepine everforest kanagawa matte-black osaka-jade; do
    base=$(hex "$name" base)
    accent=$(hex "$name" mauve)
    blue=$(hex "$name" blue)
    red=$(hex "$name" red)
    magick montage "xc:$base" "xc:$accent" "xc:$blue" "xc:$red" \
        -tile 2x2 -geometry 32x32+0+0 "swatches/$name.png"
done
