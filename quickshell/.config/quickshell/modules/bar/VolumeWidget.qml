import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's pulseaudio module.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    readonly property var icons: ["\uF026", "\uF027", "\uF028"]

    text: {
        if (Audio.muted) return "\uF026 " + Audio.volumePercent + "%";
        const idx = Audio.volumePercent >= 66 ? 2 : (Audio.volumePercent >= 33 ? 1 : 0);
        return icons[idx] + " " + Audio.volumePercent + "%";
    }
    color: Audio.muted ? Theme.overlay0 : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Audio.togglePanel(Screen.name);
            else if (mouse.button === Qt.MiddleButton) Audio.toggleMute();
            else Quickshell.execDetached(["pavucontrol"]);
        }
    }

    WheelHandler {
        onWheel: event => Audio.stepVolume(event.angleDelta.y > 0 ? 1 : -1)
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(Audio.muted ? "Muted" : "Volume: " + Audio.volumePercent + "%", point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
