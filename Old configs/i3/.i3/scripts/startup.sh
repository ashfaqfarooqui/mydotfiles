#!/bin/sh
# Startup applications for i3.

# Initialize 'kbdd' for keyboard layout handling.
kbdd

# Load Xresources file into X.
xrdb ~/.Xresources
