import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Icon glyph below is a \uXXXX escape (not a pasted Nerd Font PUA
// character) because pasted PUA glyphs have silently dropped out of QML
// string literals in this file before — escapes survive edits reliably.
// Codepoint (U+F2C7) extracted byte-exact from waybar's own config.
Text {
    text: SystemStats.tempC + "°C "
    color: SystemStats.tempC >= 80 ? Theme.red : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: Quickshell.execDetached(["ghostty", "-e", "btop"])
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show("CPU temperature: " + SystemStats.tempC + "°C") : TooltipBus.hide()
    }
}
