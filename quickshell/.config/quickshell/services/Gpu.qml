pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// AMD-only (this machine has a single integrated Strix Halo GPU, no
// discrete/NVIDIA card — confirmed via `lspci`/`nvidia-smi` absence).
// gpu_busy_percent/mem_info_vram_* are confirmed-working amdgpu sysfs
// files, but the card index (card0/card1/...) isn't stable across
// reboots/driver order, so it's resolved once at startup by scanning for
// DRIVER=amdgpu rather than hardcoded.
Singleton {
    id: root

    property bool available: false
    property real busyPercent: 0
    property real vramUsedGB: 0
    property real vramTotalGB: 0
    property string _devicePath: ""

    Process {
        id: resolveProc
        running: true
        command: ["sh", "-c", `
            for u in /sys/class/drm/card*/device/uevent; do
                if grep -q '^DRIVER=amdgpu$' "$u" 2>/dev/null; then
                    dirname "$u"
                    break
                fi
            done
        `]
        stdout: StdioCollector {
            onStreamFinished: {
                const path = this.text.trim();
                if (path) root._devicePath = path;
            }
        }
    }

    Timer {
        interval: 2000
        running: root._devicePath !== ""
        repeat: true
        triggeredOnStart: true
        onTriggered: pollProc.running = true
    }

    Process {
        id: pollProc
        command: ["sh", "-c",
            `cat "${root._devicePath}/gpu_busy_percent" 2>/dev/null; echo; ` +
            `cat "${root._devicePath}/mem_info_vram_used" 2>/dev/null; echo; ` +
            `cat "${root._devicePath}/mem_info_vram_total" 2>/dev/null`]
        stdout: StdioCollector {
            onStreamFinished: {
                const [busy, used, total] = this.text.split("\n");
                root.available = true;
                root.busyPercent = Number(busy) || 0;
                root.vramUsedGB = (Number(used) || 0) / 1073741824;
                root.vramTotalGB = (Number(total) || 0) / 1073741824;
            }
        }
    }
}
