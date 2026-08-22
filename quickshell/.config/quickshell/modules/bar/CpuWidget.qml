import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

Text {
    text: " " + SystemStats.cpuPercent + "%"
    color: SystemStats.cpuPercent >= 90 ? Theme.red : (SystemStats.cpuPercent >= 70 ? Theme.yellow : Theme.text)
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: Quickshell.execDetached(["ghostty", "-e", "btop"])
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show("CPU usage: " + SystemStats.cpuPercent + "%", point.scenePosition.x) : TooltipBus.hide()
    }
}
