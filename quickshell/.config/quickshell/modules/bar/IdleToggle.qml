import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/hypridle module.
Text {
    text: IdleInhibit.active ? "\uF070" : "\uF06E"
    color: IdleInhibit.active ? Theme.text : Theme.yellow

    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: IdleInhibit.toggle()
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(IdleInhibit.tooltip) : TooltipBus.hide()
    }
}
