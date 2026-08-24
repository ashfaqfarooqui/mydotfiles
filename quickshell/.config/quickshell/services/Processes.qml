pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Top-5 CPU consumers for VitalsPanel.qml's "Heaviest processes" section.
// Polled slower than SystemStats' /proc reads (`ps` forks and walks the
// whole process table, noticeably heavier than a plain file read).
Singleton {
    id: root

    property var topProcesses: [] // [{name, cpu}, ...]

    Timer {
        interval: 3000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: proc.running = true
    }

    Process {
        id: proc
        command: ["sh", "-c", "ps -eo comm,%cpu --sort=-%cpu --no-headers | head -5"]
        stdout: StdioCollector {
            onStreamFinished: {
                root.topProcesses = this.text.trim().split("\n").filter(l => l).map(line => {
                    const match = line.trim().match(/^(.*)\s+([\d.]+)$/);
                    return match ? { name: match[1], cpu: Number(match[2]) } : { name: line.trim(), cpu: 0 };
                });
            }
        }
    }
}
