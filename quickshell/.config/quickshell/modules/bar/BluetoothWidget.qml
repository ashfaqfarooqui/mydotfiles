import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "bluetooth" module.
Text {
    text: !Bluetooth.enabled ? "󰂲" : (Bluetooth.connectedCount > 0 ? "󰂱" : "󰂰")
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Quickshell.execDetached(["blueman-manager"]);
            else Quickshell.execDetached(["ghostty", "-e", "bash", "-c", "bluetui"]);
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            !Bluetooth.enabled ? "Bluetooth disabled" : Bluetooth.connectedCount + " connected"
        , point.scenePosition.x) : TooltipBus.hide()
    }
}
