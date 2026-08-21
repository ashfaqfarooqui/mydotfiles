-- Keybindings
local mainMod  = "SUPER"
local terminal = "ghostty"
local file_mgr = "nautilus"
local menu     = "rofi -show drun"
local browser  = "zen-browser"

local submap_resize = "RESIZE  [hjkl] resize  [0] reset  [Esc] exit"
local submap_session = "SESSION  [L]ock  [E]xit  [S]uspend  [R]eboot  [P]oweroff  [Esc] cancel"

local D = function(desc) return { description = desc } end

---------------------------------------------------------------------------
-- Application launchers
---------------------------------------------------------------------------
hl.bind(mainMod .. " + return", hl.dsp.exec_cmd(terminal),        D("Terminal"))
hl.bind(mainMod .. " + Q",      hl.dsp.window.close(),            D("Close window"))
hl.bind(mainMod .. " + F",      hl.dsp.window.fullscreen({ mode = 1 }), D("Fullscreen"))
hl.bind(mainMod .. " + E",      hl.dsp.exec_cmd(file_mgr),        D("File manager"))
hl.bind(mainMod .. " + V",      hl.dsp.window.float({ action = "toggle" }), D("Toggle float"))
hl.bind(mainMod .. " + SPACE",  hl.dsp.exec_cmd(menu),            D("App launcher"))
hl.bind(mainMod .. " + b",      hl.dsp.exec_cmd(browser),         D("Browser"))
hl.bind(mainMod .. " + o",      hl.dsp.layout("togglesplit"),     D("Toggle split"))

-- Clipboard history (image entries render as thumbnails, not a text placeholder)
hl.bind("ALT + V", hl.dsp.exec_cmd("~/.config/hypr/scripts/cliphist-picker.py"), D("Clipboard history"))

-- Lock screen
hl.bind(mainMod .. " + ALT + l", hl.dsp.exec_cmd("hyprlock"),     D("Lock screen"))

-- Screenshots (hyprshot)
hl.bind(mainMod .. " + P",      hl.dsp.exec_cmd("hyprshot -m window --clipboard-only"), D("Screenshot window"))
hl.bind(mainMod .. " + CTRL + P", hl.dsp.exec_cmd("hyprshot -m region --clipboard-only"), D("Screenshot region"))

-- Toggle waybar
hl.bind(mainMod .. " + CTRL + b", hl.dsp.exec_cmd("pkill -SIGUSR1 waybar"), D("Toggle waybar"))

-- Notification center (swaync)
hl.bind(mainMod .. " + SHIFT + n", hl.dsp.exec_cmd("swaync-client -t -sw"), D("Notifications"))

-- Rofi window switcher
hl.bind(mainMod .. " + ALT + Tab", hl.dsp.exec_cmd("rofi -show window"), D("Window switcher"))

-- Help overlay
hl.bind(mainMod .. " + slash", hl.dsp.exec_cmd("~/.config/hypr/scripts/keybind-help.sh"), D("Show keybinds"))

-- Theme picker
hl.bind(mainMod .. " + CTRL + SHIFT + SPACE", hl.dsp.exec_cmd("~/.config/hypr/scripts/theme-picker.sh"), D("Theme picker"))

-- Dictation (Voxtype)
hl.bind(mainMod .. " + CTRL + V", hl.dsp.exec_cmd("voxtype record toggle"), D("Toggle dictation"))

---------------------------------------------------------------------------
-- Window focus (vim keys)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + h", hl.dsp.focus({ direction = "left" }),  D("Focus left"))
hl.bind(mainMod .. " + j", hl.dsp.focus({ direction = "up" }),    D("Focus up"))
hl.bind(mainMod .. " + l", hl.dsp.focus({ direction = "right" }), D("Focus right"))
hl.bind(mainMod .. " + k", hl.dsp.focus({ direction = "down" }),  D("Focus down"))
hl.bind("ALT + Tab", hl.dsp.focus({ direction = "down" }),        D("Focus next"))

---------------------------------------------------------------------------
-- Window move (vim keys)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + SHIFT + h", hl.dsp.window.move({ direction = "left" }),  D("Move left"))
hl.bind(mainMod .. " + SHIFT + j", hl.dsp.window.move({ direction = "up" }),    D("Move up"))
hl.bind(mainMod .. " + SHIFT + l", hl.dsp.window.move({ direction = "right" }), D("Move right"))
hl.bind(mainMod .. " + SHIFT + k", hl.dsp.window.move({ direction = "down" }),  D("Move down"))

---------------------------------------------------------------------------
-- Window management
---------------------------------------------------------------------------
hl.bind(mainMod .. " + SHIFT + s", hl.dsp.window.swap({ direction = "left" }), D("Swap window"))
hl.bind(mainMod .. " + SHIFT + p", hl.dsp.window.pin(),             D("Pin window"))
hl.bind(mainMod .. " + ALT + c", hl.dsp.window.center(),           D("Center window"))

---------------------------------------------------------------------------
-- Resize submap (SUPER+R to enter)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + R", hl.dsp.submap(submap_resize))

hl.define_submap(submap_resize, function()
    hl.bind("h", hl.dsp.window.resize({ x = -30, y = 0, relative = true }), { repeating = true })
    hl.bind("j", hl.dsp.window.resize({ x = 0, y = 30, relative = true }), { repeating = true })
    hl.bind("k", hl.dsp.window.resize({ x = 0, y = -30, relative = true }), { repeating = true })
    hl.bind("l", hl.dsp.window.resize({ x = 30, y = 0, relative = true }), { repeating = true })

    hl.bind("0", function()
        hl.dispatch(hl.dsp.window.float({ action = "toggle" }))
        hl.dispatch(hl.dsp.window.float({ action = "toggle" }))
    end)

    hl.bind("escape", hl.dsp.submap("reset"))
    hl.bind("return", hl.dsp.submap("reset"))
    hl.bind("catchall", hl.dsp.submap("reset"))
end)

---------------------------------------------------------------------------
-- Groups (tabbed windows)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + g",           hl.dsp.group.toggle(), D("Toggle group"))
hl.bind(mainMod .. " + SHIFT + g",   hl.dsp.group.lock(),   D("Lock group"))
hl.bind(mainMod .. " + ALT + j",     hl.dsp.group.next(),   D("Group next"))
hl.bind(mainMod .. " + ALT + k",     hl.dsp.group.prev(),   D("Group prev"))

---------------------------------------------------------------------------
-- Layout cycling (dwindle <-> master)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + CTRL + l", hl.dsp.exec_cmd("~/.config/hypr/scripts/cycle-layout.sh"), D("Cycle layout"))

---------------------------------------------------------------------------
-- Cycle windows
---------------------------------------------------------------------------
hl.bind(mainMod .. " + Tab", hl.dsp.window.cycle_next(), D("Cycle windows"))

---------------------------------------------------------------------------
-- Workspaces
---------------------------------------------------------------------------
for i = 1, 10 do
    local key = i % 10
    hl.bind(mainMod .. " + " .. key,
        hl.dsp.focus({ workspace = i, on_current_monitor = true }), D("Workspace " .. i))
    hl.bind(mainMod .. " + SHIFT + " .. key,
        hl.dsp.window.move({ workspace = i }), D("Move to ws " .. i))
end

---------------------------------------------------------------------------
-- Monitor management
---------------------------------------------------------------------------
hl.bind(mainMod .. " + comma",         hl.dsp.focus({ monitor = "+1" }),            D("Focus monitor next"))
hl.bind(mainMod .. " + period",        hl.dsp.focus({ monitor = "-1" }),            D("Focus monitor prev"))
hl.bind(mainMod .. " + SHIFT + comma", hl.dsp.window.move({ monitor = "+1" }),     D("Send to other monitor"))
hl.bind(mainMod .. " + SHIFT + period", hl.dsp.window.move({ monitor = "-1" }),    D("Send to prev monitor"))
hl.bind(mainMod .. " + n",             hl.dsp.workspace.move({ monitor = "+1" }),  D("Move ws to next monitor"))
hl.bind(mainMod .. " + SHIFT + n",     hl.dsp.workspace.swap_monitors({ monitor1 = "current", monitor2 = "+1" }), D("Swap monitors"))

---------------------------------------------------------------------------
-- Special workspace (scratchpad)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + m", hl.dsp.workspace.toggle_special("magic"), D("Scratchpad"))
hl.bind(mainMod .. " + SHIFT + m", hl.dsp.window.move({ workspace = "special:magic" }), D("Move to scratchpad"))

---------------------------------------------------------------------------
-- Scroll through workspaces
---------------------------------------------------------------------------
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

---------------------------------------------------------------------------
-- Mouse binds (move/resize)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

---------------------------------------------------------------------------
-- Session/Power submap (SUPER+Escape to enter)
---------------------------------------------------------------------------
hl.bind(mainMod .. " + ESCAPE", hl.dsp.submap(submap_session))

hl.define_submap(submap_session, function()
    hl.bind("l", hl.dsp.exec_cmd("hyprlock"))
    hl.bind("e", hl.dsp.exit())
    hl.bind("s", hl.dsp.exec_cmd("systemctl suspend"))
    hl.bind("r", hl.dsp.exec_cmd("systemctl reboot"))
    hl.bind("p", hl.dsp.exec_cmd("systemctl poweroff"))
    hl.bind("escape", hl.dsp.submap("reset"))
    hl.bind("catchall", hl.dsp.submap("reset"))
end)

---------------------------------------------------------------------------
-- Multimedia keys
---------------------------------------------------------------------------
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"),
    { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),
    { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
    { locked = true, repeating = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),
    { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl s 10%+"),
    { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl s 10%-"),
    { locked = true, repeating = true })

-- Media player controls
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })
