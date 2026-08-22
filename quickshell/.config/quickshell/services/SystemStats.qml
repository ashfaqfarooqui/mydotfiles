pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces waybar's built-in temperature/memory/cpu/disk modules — Quickshell
// has no first-party sysinfo service, so this polls /proc and df directly,
// same data sources waybar's C++ probes ultimately read from.
Singleton {
    id: root

    property real cpuPercent: 0
    property real memPercent: 0
    property real diskPercent: 0
    property real tempC: 0

    property var _lastCpu: null // {idle, total}

    Timer {
        interval: 10000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            cpuProc.running = true;
            memProc.running = true;
        }
    }

    Timer {
        interval: 30000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: diskProc.running = true
    }

    // temperature module in waybar polls at its own default interval; match
    // roughly with the cpu/mem cadence since there's no interval configured
    // originally beyond waybar's built-in default.
    Timer {
        interval: 10000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: tempProc.running = true
    }

    Process {
        id: cpuProc
        command: ["sh", "-c", "grep '^cpu ' /proc/stat"]
        stdout: StdioCollector {
            onStreamFinished: {
                const fields = this.text.trim().split(/\s+/).slice(1).map(Number);
                const idle = fields[3] + (fields[4] ?? 0);
                const total = fields.reduce((a, b) => a + b, 0);
                if (root._lastCpu) {
                    const dIdle = idle - root._lastCpu.idle;
                    const dTotal = total - root._lastCpu.total;
                    if (dTotal > 0) root.cpuPercent = Math.round((1 - dIdle / dTotal) * 100);
                }
                root._lastCpu = { idle, total };
            }
        }
    }

    Process {
        id: memProc
        command: ["sh", "-c", "grep -E '^(MemTotal|MemAvailable):' /proc/meminfo"]
        stdout: StdioCollector {
            onStreamFinished: {
                const lines = this.text.trim().split("\n");
                let total = 0, avail = 0;
                for (const line of lines) {
                    const [, key, val] = line.match(/^(\w+):\s+(\d+)/) ?? [];
                    if (key === "MemTotal") total = Number(val);
                    if (key === "MemAvailable") avail = Number(val);
                }
                if (total > 0) root.memPercent = Math.round(((total - avail) / total) * 100);
            }
        }
    }

    Process {
        id: diskProc
        command: ["sh", "-c", "df -B1 --output=used,size / | tail -1"]
        stdout: StdioCollector {
            onStreamFinished: {
                const [used, size] = this.text.trim().split(/\s+/).map(Number);
                if (size > 0) root.diskPercent = Math.round((used / size) * 100);
            }
        }
    }

    Process {
        id: tempProc
        // first thermal zone is a reasonable default; override here if a
        // specific hwmon path is needed on this machine.
        command: ["sh", "-c", "cat /sys/class/thermal/thermal_zone0/temp 2>/dev/null || echo 0"]
        stdout: StdioCollector {
            onStreamFinished: root.tempC = Math.round(Number(this.text.trim()) / 1000)
        }
    }
}
