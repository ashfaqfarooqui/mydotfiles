pragma Singleton
import Quickshell

// Ported verbatim from waybar/.config/waybar/config.jsonc's "rewrite" table
// (previously duplicated between the full-bar and reduced-bar configs —
// this shared singleton is a net simplification). Icon glyph + label only;
// the inline <span foreground='#...'> hex colors are dropped in favor of
// Theme color roles applied by ActiveWindow.qml.
//
// First match wins, same semantics as the original waybar rewrite table.
Singleton {
    readonly property var rules: [
        // --- Empty desktop ---
        { pattern: /^$/, icon: "", label: "Hyprland" },

        // --- Terminals ---
        { pattern: /^~$/, icon: "", label: "Home" },
        { pattern: /^zsh$/, icon: "", label: "Terminal" },
        { pattern: /^ghostty$/, icon: "", label: "Ghostty" },
        { pattern: /^kitty$/, icon: "", label: "Kitty" },
        { pattern: /^Alacritty$/, icon: "", label: "Alacritty" },
        { pattern: /^Wezterm$/, icon: "", label: "Wezterm" },
        { pattern: /^ashfaqf@42:(.*)/, icon: "", label: "$1" },
        { pattern: /(.*)ashfaqf@42:~$/, icon: "", label: "Home" },

        // --- Browsers (specific before generic) ---
        { pattern: /(.*) — Mozilla Firefox/, icon: "󰈹", label: "$1" },
        { pattern: /(.*)Mozilla Firefox/, icon: "󰈹", label: "Firefox" },
        { pattern: /(.*) — Zen Browser/, icon: "󰈹", label: "$1" },
        { pattern: /(.*)Zen Browser/, icon: "󰈹", label: "Zen Browser" },

        // --- IDEs & Editors (specific before generic) ---
        { pattern: /(.*) - Visual Studio Code/, icon: "󰨞", label: "$1" },
        { pattern: /(.*)Visual Studio Code/, icon: "󰨞", label: "VS Code" },
        { pattern: /(.*) - Zed/, icon: "", label: "$1" },
        { pattern: /(.*)Zed$/, icon: "", label: "Zed" },
        { pattern: /(.*) — Zed/, icon: "", label: "$1" },
        { pattern: /(.*)NVIM(.*)/, icon: "", label: "$1$2" },
        { pattern: /(.*)\bNvim\b(.*)/, icon: "", label: "$1$2" },
        { pattern: /(.*) — .*Neovim$/, icon: "", label: "$1" },
        { pattern: /(.*) - Sublime Text/, icon: "", label: "$1" },
        { pattern: /(.*)Sublime Text/, icon: "", label: "Sublime Text" },
        { pattern: /(.*) - IntelliJ IDEA/, icon: "", label: "$1" },
        { pattern: /(.*)IntelliJ IDEA/, icon: "", label: "IntelliJ IDEA" },

        // --- Godot (specific before generic) ---
        { pattern: /Godot Engine - (.*)/, icon: "", label: "$1" },
        { pattern: /(.*) - Godot Engine/, icon: "", label: "$1" },
        { pattern: /^Godot$/, icon: "", label: "Godot Engine" },

        // --- File Managers ---
        { pattern: /^Dolphin$/, icon: "󰉋", label: "Dolphin" },
        { pattern: /^Nautilus$/, icon: "󰉋", label: "Nautilus" },
        { pattern: /^Thunar$/, icon: "󰉋", label: "Thunar" },
        { pattern: /(.*)\bYazi\b(.*)/, icon: "󰉋", label: "Yazi" },
        { pattern: /^\/$/, icon: "󰉋", label: "File Manager" },

        // --- Chat & Communication ---
        { pattern: /• Discord(.*)/, icon: "󰙯", label: "Discord$1" },
        { pattern: /(.*)Discord$/, icon: "󰙯", label: "Discord" },
        { pattern: /(.*)\bSlack\b(.*)/, icon: "󰒲", label: "Slack" },
        { pattern: /Telegram(.*)/, icon: "", label: "Telegram" },
        { pattern: /(.*)WhatsApp(.*)/, icon: "", label: "WhatsApp" },
        { pattern: /Zoom Meeting(.*)/, icon: "", label: "Zoom$1" },
        { pattern: /^Zoom$/, icon: "", label: "Zoom" },

        // --- Media ---
        { pattern: /(.*)\bSpotify\b(.*)/, icon: "󰓇", label: "$1$2" },
        { pattern: /^Spotify$/, icon: "󰓇", label: "Spotify" },
        { pattern: /(.*)\bVLC\b(.*)/, icon: "", label: "VLC" },
        { pattern: /(.*)\bmpv\b(.*)/, icon: "", label: "mpv" },

        // --- Games ---
        { pattern: /(.*)\bSteam\b(.*)/, icon: "󰓓", label: "Steam" },

        // --- Creative & Tools ---
        { pattern: /^GNU Image Manipulation Program$/, icon: "", label: "GIMP" },
        { pattern: /(.*)\bOBS\b(.*)/, icon: "", label: "OBS Studio" },

        // --- Image files ---
        { pattern: /(.*)\.jpg/, icon: "", label: "$1.jpg" },
        { pattern: /(.*)\.png/, icon: "", label: "$1.png" },
        { pattern: /(.*)\.svg/, icon: "", label: "$1.svg" },

        // --- Office documents ---
        { pattern: /(.*)\.docx/, icon: "", label: "$1.docx" },
        { pattern: /(.*)\.xlsx/, icon: "", label: "$1.xlsx" },
        { pattern: /(.*)\.pptx/, icon: "", label: "$1.pptx" },
        { pattern: /(.*)\.pdf/, icon: "", label: "$1.pdf" },

        // --- Misc ---
        { pattern: /^Timeshift-gtk$/, icon: "", label: "Timeshift" },
        { pattern: /^Authenticate$/, icon: "", label: "Authenticate" },

        // --- Added beyond the original waybar table: apps that weren't
        // covered there at all. Glyph codepoints cross-checked against the
        // installed JetBrainsMono Nerd Font's cmap the same way the
        // notification-panel icons were (see ControlCenter.qml's own
        // header comment on the icon-drop postmortem) — never hand-pasted.
        // Title patterns follow each app's well-known default window-title
        // convention; adjust here if a specific version differs.
        { pattern: /(.*) - Obsidian(?: v[\d.]+)?$/, icon: "\u{E6BB}", label: "$1" },
        { pattern: /^Obsidian$/, icon: "\u{E6BB}", label: "Obsidian" },

        { pattern: /(.*) - Google Chrome$/, icon: "\u{E743}", label: "$1" },
        { pattern: /^Google Chrome$/, icon: "\u{E743}", label: "Chrome" },

        { pattern: /(.*) [–-] Figma$/, icon: "\u{E7DA}", label: "$1" },
        { pattern: /^Figma$/, icon: "\u{E7DA}", label: "Figma" },

        { pattern: /^Docker Desktop$/, icon: "\u{F0868}", label: "Docker Desktop" },
        { pattern: /(.*)Docker Desktop$/, icon: "\u{F0868}", label: "Docker Desktop" },

        { pattern: /(.*) - Postman$/, icon: "\u{E86B}", label: "$1" },
        { pattern: /^Postman$/, icon: "\u{E86B}", label: "Postman" },

        { pattern: /(.*) - Mozilla Thunderbird$/, icon: "\u{F370}", label: "$1" },
        { pattern: /^Mozilla Thunderbird$/, icon: "\u{F370}", label: "Thunderbird" },

        { pattern: /(.*) - LibreOffice Writer$/, icon: "\u{F37C}", label: "$1" },
        { pattern: /^LibreOffice Writer$/, icon: "\u{F37C}", label: "LibreOffice Writer" },
        { pattern: /(.*) - LibreOffice Calc$/, icon: "\u{F378}", label: "$1" },
        { pattern: /^LibreOffice Calc$/, icon: "\u{F378}", label: "LibreOffice Calc" },
        { pattern: /(.*) - LibreOffice Impress$/, icon: "\u{F37A}", label: "$1" },
        { pattern: /^LibreOffice Impress$/, icon: "\u{F37A}", label: "LibreOffice Impress" },
    ]

    // Waybar's rewrite table uses std::regex_match (whole-string match);
    // JS's String.match() does a partial/substring search by default. Force
    // full-string semantics here in one place instead of hand-anchoring all
    // ~40 patterns above, which is what the original waybar keys actually
    // are (verbatim regex source strings, unanchored by convention there).
    function _fullMatch(pattern, title) {
        const anchored = new RegExp("^(?:" + pattern.source + ")$");
        return title.match(anchored);
    }

    // Returns { icon, label } for a window title, first match wins, falling
    // back to the raw title with no icon (mirrors waybar's default case).
    function resolve(title) {
        for (const rule of rules) {
            const m = _fullMatch(rule.pattern, title);
            if (m) {
                let label = rule.label;
                for (let i = 1; i < m.length; i++) {
                    label = label.replace("$" + i, m[i] ?? "");
                }
                return { icon: rule.icon, label: label };
            }
        }
        return { icon: "", label: title };
    }
}
