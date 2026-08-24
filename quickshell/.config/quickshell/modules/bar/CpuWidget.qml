import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    text: " " + SystemStats.cpuPercent + "%"
    color: SystemStats.cpuPercent >= 90 ? Theme.red : (SystemStats.cpuPercent >= 70 ? Theme.yellow : Theme.text)
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
        onHoveredChanged: hovered ? TooltipBus.show(
            "CPU Usage: " + SystemStats.cpuPercent + "%\nFrequency: " + SystemStats.cpuFreqGHz.toFixed(2) + "GHz"
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
