import QtQuick
import Quickshell
import Quickshell.Io
import qs.theme
import qs.config

// Replaces waybar's custom/notification module. Still shells to
// swaync-client for now since swaync stays the notification daemon until
// Phase 2 replaces it with a native NotificationServer.
Text {
    id: root
    property bool hasNotification: false
    property bool dnd: false

    text: "\uF0A2"
    color: hasNotification ? Theme.red : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    Timer {
        interval: 3000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: ["swaync-client", "-swb"]
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    const data = JSON.parse(this.text);
                    root.hasNotification = (data.count ?? 0) > 0;
                    root.dnd = !!data.dnd;
                } catch (e) {}
            }
        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Quickshell.execDetached(["swaync-client", "-t", "-sw"]);
            else Quickshell.execDetached(["swaync-client", "-d", "-sw"]);
        }
    }
}
