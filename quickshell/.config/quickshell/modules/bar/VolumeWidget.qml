import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's pulseaudio module.
Text {
    readonly property var icons: ["\uF026", "\uF027", "\uF028"]

    text: {
        if (Audio.muted) return "\uF026 " + Audio.volumePercent + "%";
        const idx = Audio.volumePercent >= 66 ? 2 : (Audio.volumePercent >= 33 ? 1 : 0);
        return icons[idx] + " " + Audio.volumePercent + "%";
    }
    color: Audio.muted ? Theme.overlay0 : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Audio.toggleMute();
            else Quickshell.execDetached(["pavucontrol"]);
        }
    }

    WheelHandler {
        onWheel: event => Audio.stepVolume(event.angleDelta.y > 0 ? 1 : -1)
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(Audio.muted ? "Muted" : "Volume: " + Audio.volumePercent + "%", point.scenePosition.x) : TooltipBus.hide()
    }
}
