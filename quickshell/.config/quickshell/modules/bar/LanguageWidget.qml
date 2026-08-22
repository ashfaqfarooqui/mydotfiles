import QtQuick
import Quickshell
import Quickshell.Io
import qs.theme
import qs.config

// Replaces waybar's hyprland/language module. Quickshell.Hyprland doesn't
// expose active keymap directly, so this polls `hyprctl devices -j` for the
// main keyboard's active_keymap, same source waybar's module reads from.
Text {
    id: root
    property string layout: ""

    // waybar's hyprland/language module has a built-in full-name -> short
    // code table for its "{short}" format; hyprctl only gives us the full
    // name (e.g. "English (US)"), so this derives a short code the same
    // way: prefer the parenthesized region code, else the first two
    // letters of the language name.
    readonly property string shortCode: {
        const m = layout.match(/\(([^)]+)\)/);
        if (m) return m[1].toUpperCase();
        return layout.slice(0, 2).toUpperCase();
    }

    text: layout ? "\uF11C " + shortCode : ""
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: ["sh", "-c", "hyprctl devices -j | jq -r '.keyboards[] | select(.main==true) | .active_keymap' | head -1"]
        stdout: StdioCollector {
            onStreamFinished: root.layout = this.text.trim()
        }
    }
}
