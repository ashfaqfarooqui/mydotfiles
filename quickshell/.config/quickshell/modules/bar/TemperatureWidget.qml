import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

// Icon glyph below is a \uXXXX escape (not a pasted Nerd Font PUA
// character) because pasted PUA glyphs have silently dropped out of QML
// string literals in this file before — escapes survive edits reliably.
// Codepoint (U+F2C7) extracted byte-exact from waybar's own config.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    text: SystemStats.tempC + "°C "
    color: SystemStats.tempC >= 80 ? Theme.red : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) SystemStats.togglePanel(Screen.name);
            else Quickshell.execDetached(["ghostty", "-e", "btop"]);
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show("CPU temperature: " + SystemStats.tempC + "°C", point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
