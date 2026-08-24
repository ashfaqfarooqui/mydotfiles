import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/hypridle module.
Text {
    id: root
    // Screen is only a valid attached property on an Item/Window \u2014 reading
    // it inside HoverHandler (a QQuickPointerHandler, not an Item) silently
    // falls back to the primary screen instead of erroring, which made
    // every tooltip report the laptop panel's screen name regardless of
    // which monitor was actually hovered. Capturing it here on the Text
    // item itself, then reading that captured value below, is the fix.
    readonly property string screenName: Screen.name

    text: IdleInhibit.active ? "\uF070" : "\uF06E"
    color: IdleInhibit.active ? Theme.text : Theme.yellow

    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        onClicked: IdleInhibit.toggle()
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(IdleInhibit.tooltip, point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
