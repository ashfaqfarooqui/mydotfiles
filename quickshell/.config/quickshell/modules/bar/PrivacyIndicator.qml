import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's privacy indicator module — separate mic and
// camera/screenshare icons, see services/Privacy.qml for the detection
// logic and its documented limits.
Row {
    spacing: 4

    Text {
        visible: Privacy.micInUse
        text: "󰍬"
        color: Theme.red
        font.family: Config.fontFamily
        font.pixelSize: Config.fontSize

        HoverHandler {
            onHoveredChanged: hovered ? TooltipBus.show("Microphone in use", point.scenePosition.x) : TooltipBus.hide()
        }
    }

    Text {
        visible: Privacy.videoInUse
        text: "󰃑"
        color: Theme.red
        font.family: Config.fontFamily
        font.pixelSize: Config.fontSize

        HoverHandler {
            onHoveredChanged: hovered ? TooltipBus.show("Camera or screen share in use", point.scenePosition.x) : TooltipBus.hide()
        }
    }
}
