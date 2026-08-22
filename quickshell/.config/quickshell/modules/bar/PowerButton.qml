import QtQuick
import Quickshell
import qs.theme
import qs.config

// Replaces waybar's custom/power module. Still calls the existing
// power-menu.sh (rofi-backed) for now — a native PowerMenuUI.qml lands in
// Phase 3, this just gets the bar itself onto Quickshell first.
Text {
    text: "\uF011"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: Quickshell.execDetached([Quickshell.env("HOME") + "/.config/waybar/scripts/power-menu.sh"])
    }
}
