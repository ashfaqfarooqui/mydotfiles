import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/ws + hyprland/workspaces modules: a leading
// icon, then buttons 1-10 (persistent, like waybar's persistent-workspaces),
// highlighting the focused one. Scroll to switch, same as the original
// on-scroll-up/down dispatchers.
Row {
    id: root
    spacing: 2

    readonly property var icons: ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

    Text {
        text: "\uF303"
        color: Theme.overlay0
        font.family: Config.fontFamily
        font.pixelSize: Config.fontSize
        anchors.verticalCenter: parent.verticalCenter
        rightPadding: 4
    }

    Repeater {
        model: 10
        delegate: Text {
            required property int index
            readonly property int wsId: index + 1
            readonly property bool isFocused: Hypr.focusedWorkspace?.id === wsId

            text: root.icons[index]
            color: isFocused ? Theme.blue : Theme.overlay0
            font.family: Config.fontFamily
            font.pixelSize: Config.fontSize
            font.bold: isFocused
            leftPadding: 4
            rightPadding: 4

            MouseArea {
                anchors.fill: parent
                onClicked: Hypr.dispatch("workspace " + wsId)
            }
        }
    }

    WheelHandler {
        target: root
        onWheel: event => {
            Hypr.dispatch(event.angleDelta.y < 0 ? "workspace +1" : "workspace -1");
        }
    }
}
