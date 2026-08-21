#!/usr/bin/env python3
# Rewrite starship.toml's [palettes.catppuccin_mocha] color values in place from
# a generated hypr-theme.lua (M.name = "rgb(hex)" lines). The rest of the file
# (format, modules) is untouched — starship has no include mechanism, so this
# is a surgical block replace rather than a full-file template.
import re
import sys

hypr_theme_path, starship_path = sys.argv[1], sys.argv[2]

colors = {}
with open(hypr_theme_path) as f:
    for line in f:
        m = re.match(r'M\.(\w+) = "rgb\(([0-9a-f]{6})\)"', line)
        if m:
            colors[m.group(1)] = m.group(2)

with open(starship_path) as f:
    content = f.read()


def replace_color(m):
    name = m.group(1)
    hex_value = colors.get(name)
    if hex_value is None:
        return m.group(0)
    return f'{name} = "#{hex_value}"'


start = content.index("[palettes.catppuccin_mocha]")
end = content.index("\n[", start + 1)
block = content[start:end]
new_block = re.sub(r'(\w+) = "#[0-9a-fA-F]{6}"', replace_color, block)
content = content[:start] + new_block + content[end:]

with open(starship_path, "w") as f:
    f.write(content)
