pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces waybar's custom/netspeed module — reuses the existing script
// (it already maintains its own /tmp state file for the rate calculation).
Singleton {
    id: root
    property string text: "↑-.--MB\n↓-.--MB"

    readonly property string script: Quickshell.env("HOME") + "/.config/waybar/scripts/netspeed.sh"

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: [root.script]
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    root.text = JSON.parse(this.text).text ?? root.text;
                } catch (e) {}
            }
        }
    }
}
