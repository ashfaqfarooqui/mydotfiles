import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "bluetooth" module.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    text: !Bluetooth.enabled ? "󰂲" : (Bluetooth.connectedCount > 0 ? "󰂱" : "󰂰")
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Bluetooth.togglePanel(Screen.name);
            else Quickshell.execDetached(["ghostty", "-e", "bash", "-c", "bluetui"]);
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            !Bluetooth.enabled ? "Bluetooth disabled" : Bluetooth.connectedCount + " connected"
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
