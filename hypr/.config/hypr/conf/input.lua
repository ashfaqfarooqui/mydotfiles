-- Input: keyboard, touchpad, cursor, per-device config
hl.config({
    input = {
        kb_layout          = "us, se",
        kb_options         = "grp:alt_shift_toggle",
        numlock_by_default = true,
        follow_mouse       = 1,
        sensitivity        = 0.75,
        repeat_rate        = 50,
        repeat_delay       = 300,

        touchpad           = {
            natural_scroll          = true,
            disable_while_typing    = false,
            middle_button_emulation = false,
            drag_lock               = false,
            tap_to_click            = true,
        },

        touchdevice        = {
            enabled = false,
        },
    },

    cursor = {
        sync_gsettings_theme = true,
        enable_hyprcursor    = true,
    },
})

-- 3-finger horizontal workspace swipe is registered by dms/binds.lua (identical).
