-- Look and feel: general, decoration, animations, layouts, misc, xwayland
local theme = require("theme")

-- Hyprland's gradient border syntax needs rgba(), not rgb() — theme.lua only has rgb().
local function rgba(rgb_str)
    return (rgb_str:gsub("^rgb%(", "rgba("):gsub("%)$", "ff)"))
end

---------------------------------------------------------------------------
-- Options
---------------------------------------------------------------------------
hl.config({
    general = {
        gaps_in          = 5,
        gaps_out         = 20,
        border_size      = 2,
        col              = {
            -- Gradient (not a flat color) so the "borderangle" animation below has
            -- something to actually rotate.
            active_border   = { colors = { rgba(theme.mauve), rgba(theme.pink), rgba(theme.blue) }, angle = 45 },
            inactive_border = theme.surface2,
        },
        resize_on_border = true,
        allow_tearing    = false,
        layout           = "dwindle",
    },

    decoration = {
        rounding           = 10,
        active_opacity     = 1.0,
        inactive_opacity   = 0.95,
        fullscreen_opacity = 1.0,
        dim_inactive       = true,
        dim_strength       = 0.15,
        dim_special        = 0.15,
        shadow           = {
            enabled        = true,
            range          = 20,
            render_power   = 2,
            color          = "rgba(1e1e2eee)",
            color_inactive = "rgba(1e1e2e00)",
        },
        blur             = {
            enabled    = true,
            size       = 6,
            passes     = 3,
            vibrancy   = 0.1696,
            popups     = true,
            popups_ignorealpha = 0.2,
        },
    },

    dwindle = {
        preserve_split = true,
    },

    master = {
        new_status = "master",
    },

    misc = {
        force_default_wallpaper = -1,
        disable_hyprland_logo   = true,
        focus_on_activate       = true,
        enable_swallow          = true,
        swallow_regex           = "^(ghostty|Alacritty|kitty|foot)$",
        mouse_move_enables_dpms = true,
        key_press_enables_dpms  = true,
    },

    xwayland = {
        force_zero_scaling   = true,
        use_nearest_neighbor = true,
        enabled              = true,
    },
})

---------------------------------------------------------------------------
-- Animation curves
---------------------------------------------------------------------------
hl.curve("snappy", { type = "bezier", points = { { 0.15, 0.9 }, { 0.3, 1 } } })
hl.curve("quick",  { type = "bezier", points = { { 0.15, 0 }, { 0.1, 1 } } })
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })

---------------------------------------------------------------------------
-- Animations
---------------------------------------------------------------------------
hl.animation({ leaf = "global",               enabled = true, speed = 4,   bezier = "default" })
hl.animation({ leaf = "border",               enabled = true, speed = 4,   bezier = "snappy" })
hl.animation({ leaf = "borderangle",          enabled = true, speed = 4,   bezier = "snappy" })
hl.animation({ leaf = "windows",              enabled = true, speed = 4,   bezier = "snappy" })
hl.animation({ leaf = "windowsIn",            enabled = true, speed = 3,   bezier = "snappy", style = "popin 87%" })
hl.animation({ leaf = "windowsOut",           enabled = true, speed = 3,   bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "windowsMove",          enabled = true, speed = 4,   bezier = "snappy" })
hl.animation({ leaf = "fade",                 enabled = true, speed = 3,   bezier = "quick" })
hl.animation({ leaf = "fadeIn",               enabled = true, speed = 3,   bezier = "quick" })
hl.animation({ leaf = "fadeOut",              enabled = true, speed = 2,   bezier = "quick" })
hl.animation({ leaf = "layers",               enabled = true, speed = 3,   bezier = "snappy" })
hl.animation({ leaf = "layersIn",             enabled = true, speed = 3,   bezier = "snappy" })
hl.animation({ leaf = "layersOut",            enabled = true, speed = 2,   bezier = "linear" })
hl.animation({ leaf = "fadeLayersIn",         enabled = true, speed = 3,   bezier = "quick" })
hl.animation({ leaf = "fadeLayersOut",        enabled = true, speed = 2,   bezier = "quick" })
hl.animation({ leaf = "workspaces",           enabled = true, speed = 4,   bezier = "snappy",  style = "slide" })
hl.animation({ leaf = "workspacesIn",         enabled = true, speed = 4,   bezier = "snappy",  style = "slide" })
hl.animation({ leaf = "workspacesOut",        enabled = true, speed = 4,   bezier = "snappy",  style = "slide" })
hl.animation({ leaf = "specialWorkspace",     enabled = true, speed = 4,   bezier = "snappy",  style = "slidevert" })
hl.animation({ leaf = "specialWorkspaceIn",   enabled = true, speed = 4,   bezier = "snappy",  style = "slidevert" })
hl.animation({ leaf = "specialWorkspaceOut",  enabled = true, speed = 4,   bezier = "snappy",  style = "slidevert" })
