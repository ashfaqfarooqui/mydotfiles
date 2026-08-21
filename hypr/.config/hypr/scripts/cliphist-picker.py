#!/usr/bin/env python3
# Clipboard picker: cliphist list -> rofi (with image thumbnails) -> wl-copy.
# Image entries in `cliphist list` look like "7045\t[[ binary data 89 KiB png 1080x1920 ]]" —
# decode those to a cache file and tag the row with rofi's "\0icon\x1f<path>" convention
# so `-show-icons` renders an actual thumbnail instead of that placeholder text.
import os
import re
import subprocess
import sys

cache_dir = os.path.join(os.environ.get("XDG_RUNTIME_DIR", "/tmp"), "cliphist-thumbs")
os.makedirs(cache_dir, exist_ok=True)

img_re = re.compile(r"^(\d+)\t\[\[ binary data .*?(png|jpe?g|bmp|gif|webp) ", re.IGNORECASE)


def thumbnail_for(entry_id: str, ext: str) -> str:
    path = os.path.join(cache_dir, f"{entry_id}.{ext}")
    if not os.path.exists(path):
        proc = subprocess.run(["cliphist", "decode", entry_id], capture_output=True, check=True)
        with open(path, "wb") as f:
            f.write(proc.stdout)
    return path


def list_entries():
    result = subprocess.run(["cliphist", "list"], capture_output=True, text=True, check=True)
    lines = []
    for line in result.stdout.splitlines():
        match = img_re.match(line)
        if match:
            entry_id, ext = match.groups()
            thumb = thumbnail_for(entry_id, ext)
            lines.append(f"{line}\0icon\x1f{thumb}")
        else:
            lines.append(line)
    return lines


def main():
    lines = list_entries()
    proc = subprocess.run(
        ["rofi", "-dmenu", "-show-icons", "-p", "Clipboard"],
        input="\n".join(lines),
        capture_output=True,
        text=True,
    )
    selected = proc.stdout.strip()
    if not selected:
        return
    entry_id = selected.split("\t", 1)[0]
    decode = subprocess.run(["cliphist", "decode", entry_id], capture_output=True, check=True)
    subprocess.run(["wl-copy"], input=decode.stdout)


if __name__ == "__main__":
    main()
