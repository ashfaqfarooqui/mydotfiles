import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/backlight module.
Text {
    text: "󰃟 " + Brightness.percent + "%"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        onClicked: Brightness.togglePanel(Screen.name)
    }

    WheelHandler {
        onWheel: event => Brightness.step(event.angleDelta.y > 0 ? 5 : -5)
    }
}
