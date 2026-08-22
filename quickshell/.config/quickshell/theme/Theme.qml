pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Loads theme/theme.json (same 26-slot Catppuccin color-role schema as
// theme/palettes/*.json in the dotfiles repo) and live-reloads on change.
// theme/justfile apply will copy the whiskers-rendered output here once
// theme/quickshell.tera exists (Phase 4).
Singleton {
    id: root

    FileView {
        id: paletteFile
        path: Qt.resolvedUrl("./theme.json")
        watchChanges: true
        onFileChanged: reload()
    }

    readonly property var palette: {
        try {
            return JSON.parse(paletteFile.text());
        } catch (e) {
            return {};
        }
    }

    readonly property color base: palette.base ? ("#" + palette.base) : "#1e1e2e"
    readonly property color mantle: palette.mantle ? ("#" + palette.mantle) : "#181825"
    readonly property color crust: palette.crust ? ("#" + palette.crust) : "#11111b"
    readonly property color surface0: palette.surface0 ? ("#" + palette.surface0) : "#313244"
    readonly property color surface1: palette.surface1 ? ("#" + palette.surface1) : "#45475a"
    readonly property color surface2: palette.surface2 ? ("#" + palette.surface2) : "#585b70"
    readonly property color overlay0: palette.overlay0 ? ("#" + palette.overlay0) : "#6c7086"
    readonly property color subtext0: palette.subtext0 ? ("#" + palette.subtext0) : "#a6adc8"
    readonly property color subtext1: palette.subtext1 ? ("#" + palette.subtext1) : "#bac2de"
    readonly property color text: palette.text ? ("#" + palette.text) : "#cdd6f4"
    readonly property color red: palette.red ? ("#" + palette.red) : "#f38ba8"
    readonly property color peach: palette.peach ? ("#" + palette.peach) : "#fab387"
    readonly property color yellow: palette.yellow ? ("#" + palette.yellow) : "#f9e2af"
    readonly property color green: palette.green ? ("#" + palette.green) : "#a6e3a1"
    readonly property color teal: palette.teal ? ("#" + palette.teal) : "#94e2d5"
    readonly property color sky: palette.sky ? ("#" + palette.sky) : "#89dceb"
    readonly property color blue: palette.blue ? ("#" + palette.blue) : "#89b4fa"
    readonly property color lavender: palette.lavender ? ("#" + palette.lavender) : "#b4befe"
    readonly property color mauve: palette.mauve ? ("#" + palette.mauve) : "#cba6f7"
    readonly property color pink: palette.pink ? ("#" + palette.pink) : "#f5c2e7"
}
