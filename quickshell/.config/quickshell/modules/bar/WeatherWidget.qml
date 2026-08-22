import QtQuick
import qs.theme
import qs.config
import qs.services

Text {
    text: Weather.text + "°"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(Weather.tooltip, point.scenePosition.x) : TooltipBus.hide()
    }
}
