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

    text: "󰘚 " + SystemStats.memPercent + "%"
    color: SystemStats.memPercent >= 90 ? Theme.red : (SystemStats.memPercent >= 75 ? Theme.yellow : Theme.text)
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
            "Memory Used: " + SystemStats.memUsedGB.toFixed(1) + " GB / " + SystemStats.memTotalGB.toFixed(1) + " GB\n" +
            "Swap: " + SystemStats.swapUsedGB.toFixed(1) + " GB / " + SystemStats.swapTotalGB.toFixed(1) + " GB"
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
