pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces waybar's custom/weather module — keeps shelling to wttrbar,
// same as the original exec, just parsed here instead of by waybar.
Singleton {
    id: root
    property string text: "..."
    property string tooltip: ""

    Timer {
        interval: 60000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: ["wttrbar", "--location", "boras,Sweden"]
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    const data = JSON.parse(this.text);
                    root.text = data.text ?? "";
                    root.tooltip = data.tooltip ?? "";
                } catch (e) {}
            }
        }
    }
}
