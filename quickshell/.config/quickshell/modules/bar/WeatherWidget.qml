import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    text: Weather.text + "°"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(Weather.tooltip, point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
