import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

Text {
    text: "󰘚 " + SystemStats.memPercent + "%"
    color: SystemStats.memPercent >= 90 ? Theme.red : (SystemStats.memPercent >= 75 ? Theme.yellow : Theme.text)
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: Quickshell.execDetached(["ghostty", "-e", "btop"])
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show("Memory used: " + SystemStats.memPercent + "%") : TooltipBus.hide()
    }
}
