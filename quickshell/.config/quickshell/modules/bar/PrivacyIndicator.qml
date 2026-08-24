import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Replaces waybar's privacy indicator module — separate mic, camera, and
// screen-share icons, see services/Privacy.qml for the detection logic
// (camera and screenshare are tracked independently there, via two
// different mechanisms) and its documented limits.
Row {
    spacing: 4

    Text {
        id: micText
        // Screen must be captured here (a real Item), not read inside
        // HoverHandler below — see IdleToggle.qml for why.
        readonly property string screenName: Screen.name

        visible: Privacy.micInUse
        text: "󰍬"
        color: Theme.red
        font.family: Config.fontFamily
        font.pixelSize: Settings.fontSize
        font.weight: Config.fontWeight

        HoverHandler {
            onHoveredChanged: hovered ? TooltipBus.show("Microphone in use", point.scenePosition.x, micText.screenName) : TooltipBus.hide()
        }
    }

    Text {
        id: cameraText
        readonly property string screenName: Screen.name

        // mdi-webcam — distinct from the generic camera glyph, and from
        // the monitor-share glyph below, so all three privacy icons read
        // differently from each other at a glance.
        visible: Privacy.cameraInUse
        text: "󰖠"
        color: Theme.red
        font.family: Config.fontFamily
        font.pixelSize: Settings.fontSize
        font.weight: Config.fontWeight

        HoverHandler {
            onHoveredChanged: hovered ? TooltipBus.show("Camera in use", point.scenePosition.x, cameraText.screenName) : TooltipBus.hide()
        }
    }

    Text {
        id: shareText
        readonly property string screenName: Screen.name

        // mdi-monitor-share
        visible: Privacy.screenshareInUse
        text: "󱒃"
        color: Theme.red
        font.family: Config.fontFamily
        font.pixelSize: Settings.fontSize
        font.weight: Config.fontWeight

        HoverHandler {
            onHoveredChanged: hovered ? TooltipBus.show("Screen share in use", point.scenePosition.x, shareText.screenName) : TooltipBus.hide()
        }
    }
}
