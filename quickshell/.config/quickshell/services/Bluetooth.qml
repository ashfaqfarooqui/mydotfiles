pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Bluetooth as QB

// Replaces waybar's "bluetooth" module. Uses the confirmed native
// Quickshell.Bluetooth binding for power state, and bluetoothctl (same
// backend blueman-manager/bluetui already use) for the connected count,
// since BluetoothDevice's exact filtered-by-adapter surface isn't fully
// confirmed yet.

Singleton {
    id: root
    readonly property bool enabled: QB.Bluetooth.defaultAdapter?.enabled ?? false
    property int connectedCount: 0

    Timer {
        interval: 5000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: ["sh", "-c", "bluetoothctl devices Connected | wc -l"]
        stdout: StdioCollector {
            onStreamFinished: root.connectedCount = parseInt(this.text.trim()) || 0
        }
    }
}
