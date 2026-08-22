pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces waybar's "network" module. Shells to nmcli (same tool nm-applet
// and nmtui already sit on top of) rather than the native
// Quickshell.Networking binding, since that module's exact property surface
// isn't fully confirmed yet — revisit as a native-binding refinement later.
Singleton {
    id: root
    property string kind: "disconnected" // wifi | ethernet | disconnected
    property int signalStrength: 0
    property string ssid: ""

    Timer {
        interval: 10000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: ["sh", "-c", "nmcli -t -f TYPE,STATE,CONNECTION device status | grep -E '^(wifi|ethernet):connected' | head -1"]
        stdout: StdioCollector {
            onStreamFinished: {
                const line = this.text.trim();
                if (!line) {
                    root.kind = "disconnected";
                    return;
                }
                const [type] = line.split(":");
                root.kind = type;
                if (type === "wifi") wifiInfo.running = true;
            }
        }
    }

    Process {
        id: wifiInfo
        command: ["sh", "-c", "nmcli -t -f active,signal,ssid dev wifi | grep '^yes'"]
        stdout: StdioCollector {
            onStreamFinished: {
                const parts = this.text.trim().split(":");
                if (parts.length >= 3) {
                    root.signalStrength = parseInt(parts[1]) || 0;
                    root.ssid = parts.slice(2).join(":");
                }
            }
        }
    }
}
