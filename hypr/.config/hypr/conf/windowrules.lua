-- Window rules and layer rules

---------------------------------------------------------------------------
-- Helper
---------------------------------------------------------------------------

--- Assign an application to a workspace silently by class name regex.
--- Reduces repetition for the common pattern of pinning apps to workspaces.
--- @param class_regex string  Regex pattern matching the window class
--- @param ws number|string    Target workspace number
local function assign_workspace(class_regex, ws)
    hl.window_rule({ match = { class = class_regex }, workspace = ws .. " silent" })
end

---------------------------------------------------------------------------
-- Workspace assignments
--   1: Web       2: Notes    3: Code
--   4: Comms     5: Research  9: Todo
---------------------------------------------------------------------------
assign_workspace("^(Firefox)$", 1)
assign_workspace("^(zen)$", 1)
assign_workspace("^(chromium)$", 1)
assign_workspace("^(obsidian)$", 2)
assign_workspace("^(emacs)$", 3)
assign_workspace("^(codium)$", 3)
assign_workspace("^(dev.zed.Zed)$", 3)
assign_workspace("^(teams-for-linux)$",  4)
assign_workspace("^(Zoom)$",             4)
assign_workspace("^(FFPWA-.*)$",         4)
assign_workspace("^(Zotero)$", 5)
assign_workspace("^(Marvin)$", 9)

---------------------------------------------------------------------------
-- Float rules
---------------------------------------------------------------------------
-- Title-based floats
hl.window_rule({ match = { title = "^(Extension)$" }, float = true })
hl.window_rule({ match = { title = "^(Volume Control)$" }, float = true, size = "800 600" })
hl.window_rule({ match = { title = "^(Media viewer)$" }, float = true })
hl.window_rule({ match = { title = "^(Open Files)$" }, size = "1000 700" })
hl.window_rule({ match = { title = "^(Save File)$" }, size = "1000 700" })
hl.window_rule({ match = { title = "^(Bluetooth)" }, float = true })
hl.window_rule({ match = { class = "^(steam)$" }, float = true })
hl.window_rule({ match = { title = "^(guifetch)$" }, float = true })
hl.window_rule({ match = { class = "^(Nextcloud)$" }, float = true })
hl.window_rule({ match = { class = "^(blueman-manager)$" }, float = true })
hl.window_rule({ match = { title = "^(nm-connection-editor)$" }, float = true })
hl.window_rule({ match = { title = "^(Microsoft-edge)$" }, tile = true })

hl.window_rule({ match = { class = "^(Zotero)$" }, suppress_event = "activate" })
-- Class-based floats (common dialog patterns)
local float_classes = {
    "file_progress", "confirm", "dialog", "download",
    "notification", "error", "splash", "confirmreset",
}
for _, cls in ipairs(float_classes) do
    hl.window_rule({ match = { class = cls }, float = true })
end

-- Title-based floats (file dialogs)
local float_titles = {
    "pavucontrol-qt", "pavucontrol", "file-roller",
}
for _, title in ipairs(float_titles) do
    hl.window_rule({ match = { title = title }, float = true })
end

-- File dialog centering + float
local centered_dialogs = {
    "^(Open File)(.*)$",
    "^(Select a File)(.*)$",
    "^(Choose wallpaper)(.*)$",
    "^(Open Folder)(.*)$",
    "^(Save As)(.*)$",
    "^(Library)(.*)$",
    "^(File Upload)(.*)$",
}
for _, pattern in ipairs(centered_dialogs) do
    hl.window_rule({ match = { title = pattern }, float = true, center = true })
end

-- Picture-in-Picture (browser)
hl.window_rule({
    match = { title = "^(Picture(-| )in(-| )[Pp]icture)(.*)$" },
    float = true,
    pin   = true,
})

-- Warp terminal -> tile
hl.window_rule({ match = { class = "dev.warp.Warp" }, tile = true })

-- Tearing for Steam games
hl.window_rule({ match = { class = "steam_app" }, immediate = true })

-- Dim inactive tiled windows
hl.window_rule({ match = { focus = 0, float = false }, opacity = 0.9 })

-- Prevent Steam from stealing focus on sub-windows
hl.window_rule({ match = { class = "^(steam)$" }, suppress_event = "activate" })

-- No shadow for tiled windows
hl.window_rule({ match = { float = false }, no_shadow = true })

---------------------------------------------------------------------------
-- Smart gaps: no gaps when only one tiled window
---------------------------------------------------------------------------
hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]", gaps_out = 0, gaps_in = 0 })

hl.window_rule({ match = { float = false, workspace = "w[tv1]" }, border_size = 0, rounding = 0 })
hl.window_rule({ match = { float = false, workspace = "f[1]" }, border_size = 0, rounding = 0 })

---------------------------------------------------------------------------
-- Layer rules
---------------------------------------------------------------------------
hl.layer_rule({ match = { namespace = ".*" }, xray = 1 })
hl.layer_rule({ match = { namespace = "walker" }, no_anim = true })
hl.layer_rule({ match = { namespace = "selection" }, no_anim = true })
hl.layer_rule({ match = { namespace = "overview" }, no_anim = true, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "anyrun" }, no_anim = true })
hl.layer_rule({ match = { namespace = "indicator.*" }, no_anim = true, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "osk" }, no_anim = true, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "hyprpicker" }, no_anim = true })
hl.layer_rule({ match = { namespace = "shell:*" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "noanim" }, no_anim = true })
hl.layer_rule({ match = { namespace = "gtk-layer-shell" }, blur = true, ignore_alpha = 0 })
hl.layer_rule({ match = { namespace = "launcher" }, blur = true, ignore_alpha = 0.5 })
hl.layer_rule({ match = { namespace = "notifications" }, blur = true, ignore_alpha = 0.69 })

-- AGS layers
hl.layer_rule({ match = { namespace = "sideleft.*" }, animation = "slide left" })
hl.layer_rule({ match = { namespace = "sideright.*" }, animation = "slide right" })
hl.layer_rule({ match = { namespace = "session" }, blur = true })

-- Bar / dock / widgets
hl.layer_rule({ match = { namespace = "bar" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "corner.*" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "dock" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "cheatsheet" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "sideright" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "sideleft" }, blur = true, ignore_alpha = 0.6 })
hl.layer_rule({ match = { namespace = "indicator*" }, blur = true, ignore_alpha = 0.6 })

-- Swaync
hl.layer_rule({ match = { namespace = "swaync-control-center" }, blur = true, ignore_alpha = 0.5 })
hl.layer_rule({ match = { namespace = "swaync-notification-window" }, blur = true, ignore_alpha = 0.5 })
