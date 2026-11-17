-- Hyprland configuration (Lua format, 0.55+)
-- Main entry point: loads all configuration modules in order.

-- DMS-managed fragments first so our own conf/* can override them.
-- dms.outputs is skipped: conf.monitors has explicit per-output rules.
require("dms.colors")
require("dms.layout")

require("conf.env") -- Environment variables (set before anything else)
require("conf.monitors") -- Monitor layout
require("conf.options") -- General, decoration, animations, layouts, misc, xwayland
require("conf.input") -- Keyboard, touchpad, cursor, per-device
require("conf.autostart") -- Startup applications
require("dms.binds") -- DMS default binds (shell IPC, media, screenshots)
require("dms.binds-user")
require("conf.dms") -- Unbind DMS defaults that clash with ours, relocate the rest
require("conf.keybindings") -- Keybinds
require("conf.windowrules") -- Window rules + layer rules
