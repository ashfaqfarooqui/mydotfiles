pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces waybar's custom/hypridle module. Reuses the existing script
// verbatim rather than reimplementing its pgrep/pkill logic in QML.
Singleton {
    id: root
    property bool active: true // true = idle mode active (hypridle running)
    property string tooltip: ""

    readonly property string script: Quickshell.env("HOME") + "/.config/waybar/scripts/hypridle-toggle.sh"

    Timer {
        interval: 5000
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
                    const data = JSON.parse(this.text);
                    root.active = data.class === "active";
                    root.tooltip = data.tooltip ?? "";
                } catch (e) {}
            }
        }
    }

    function toggle() {
        Quickshell.execDetached([root.script, "toggle"]);
        pollSoon.running = true;
    }

    Timer {
        id: pollSoon
        interval: 300
        onTriggered: poll.running = true
    }
}
