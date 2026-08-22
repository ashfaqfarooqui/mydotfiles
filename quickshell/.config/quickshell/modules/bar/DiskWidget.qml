import QtQuick
import qs.theme
import qs.config
import qs.services

Text {
    text: "󰨣 " + SystemStats.diskPercent + "%"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            "Used: " + SystemStats.diskUsed + " / " + SystemStats.diskTotal + " (" + SystemStats.diskPercent + "%)\n" +
            "Free: " + SystemStats.diskFree
        , point.scenePosition.x) : TooltipBus.hide()
    }
}
