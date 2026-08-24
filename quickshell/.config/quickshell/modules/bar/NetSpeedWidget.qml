import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/netspeed module. Waybar renders this as a
// genuine two-line label (GTK multi-line markup); stack the same two lines
// vertically here instead of joining them with a space, which read as
// "up/down side by side" rather than the original up-over-down layout.
Column {
    readonly property var lines: NetSpeed.text.split("\n")
    spacing: 0

    Text {
        text: lines[0] ?? ""
        color: Theme.text
        font.family: Config.fontFamily
        font.pixelSize: Settings.fontSize - 3
        font.weight: Config.fontWeight
    }

    Text {
        text: lines[1] ?? ""
        color: Theme.text
        font.family: Config.fontFamily
        font.pixelSize: Settings.fontSize - 3
        font.weight: Config.fontWeight
    }
}
