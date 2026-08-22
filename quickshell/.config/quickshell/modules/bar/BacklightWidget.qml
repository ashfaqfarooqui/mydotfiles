import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/backlight module.
Text {
    text: "󰃟 " + Brightness.percent + "%"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    WheelHandler {
        onWheel: event => Brightness.step(event.angleDelta.y > 0 ? 5 : -5)
    }
}
