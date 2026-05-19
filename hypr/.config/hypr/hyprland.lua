-- Hyprland configuration (Lua format, 0.55+)
-- Main entry point: loads all configuration modules in order.

require("conf.env")         -- Environment variables (set before anything else)
require("conf.monitors")    -- Monitor layout
require("conf.options")     -- General, decoration, animations, layouts, misc, xwayland
require("conf.input")       -- Keyboard, touchpad, cursor, per-device
require("conf.autostart")   -- Startup applications
require("conf.keybindings") -- Keybinds
require("conf.windowrules") -- Window rules + layer rules
