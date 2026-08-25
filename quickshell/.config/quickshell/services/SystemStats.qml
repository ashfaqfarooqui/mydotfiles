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
    property real cpuFreqGHz: 0
    property real memPercent: 0
    property real memUsedGB: 0
    property real memTotalGB: 0
    property real swapUsedGB: 0
    property real swapTotalGB: 0
    property real diskPercent: 0
    property string diskUsed: ""
    property string diskTotal: ""
    property string diskFree: ""
    property real tempC: 0
    // One entry per logical core, 0-100 — feeds VitalsPanel.qml's per-core
    // bar row. Empty until the first poll resolves.
    property var perCorePercent: []

    // Vitals panel visibility — same panelVisible/panelScreenName bus as
    // Brightness.qml/Battery.qml, shared by the 4 separate stat widgets
    // (Cpu/Memory/Disk/Temperature) so they all open the one combined panel.
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            Network.hidePanel();
            Bluetooth.hidePanel();
            Battery.hidePanel();
            Brightness.hidePanel();
            Audio.hidePanel();
            Calendar.hidePanel();
            AgentsUsage.hidePanel();
            Tailscale.hidePanel();
            Weather.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }

    property var _lastCpu: null // {idle, total}
    property var _lastCores: null // [{idle, total}, ...] per core

    // Graphics/disk temperature and fan RPM aren't exposed on every machine
    // (confirmed: this one has no fan*_input anywhere in hwmon at all) — so
    // these are probed once at startup rather than hardcoded, and
    // VitalsPanel.qml hides each tile whose *Available is false instead of
    // showing a fake/zero reading.
    property bool graphicsTempAvailable: false
    property real graphicsTempC: 0
    property bool diskTempAvailable: false
    property real diskTempC: 0
    property bool fanAvailable: false
    property int fanRpm: 0
    property string _gpuTempPath: ""
    property string _diskTempPath: ""
    property string _fanPath: ""
    property string _cpuTempPath: ""

    Timer {
        interval: 10000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            cpuProc.running = true;
            cpuFreqProc.running = true;
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
        // '^cpu ' is the aggregate line, '^cpu[0-9]' the per-core lines
        // (needed for VitalsPanel.qml's per-core bar row).
        command: ["sh", "-c", "grep '^cpu' /proc/stat"]
        stdout: StdioCollector {
            onStreamFinished: {
                function parse(line) {
                    const fields = line.trim().split(/\s+/).slice(1).map(Number);
                    const idle = fields[3] + (fields[4] ?? 0);
                    const total = fields.reduce((a, b) => a + b, 0);
                    return { idle, total };
                }

                const lines = this.text.trim().split("\n");
                const agg = parse(lines[0]);
                if (root._lastCpu) {
                    const dIdle = agg.idle - root._lastCpu.idle;
                    const dTotal = agg.total - root._lastCpu.total;
                    if (dTotal > 0) root.cpuPercent = Math.round((1 - dIdle / dTotal) * 100);
                }
                root._lastCpu = agg;

                const cores = lines.slice(1).map(parse);
                if (root._lastCores && root._lastCores.length === cores.length) {
                    root.perCorePercent = cores.map((c, i) => {
                        const prev = root._lastCores[i];
                        const dIdle = c.idle - prev.idle;
                        const dTotal = c.total - prev.total;
                        return dTotal > 0 ? Math.round((1 - dIdle / dTotal) * 100) : 0;
                    });
                }
                root._lastCores = cores;
            }
        }
    }

    Process {
        id: memProc
        command: ["sh", "-c", "grep -E '^(MemTotal|MemAvailable|SwapTotal|SwapFree):' /proc/meminfo"]
        stdout: StdioCollector {
            onStreamFinished: {
                const lines = this.text.trim().split("\n");
                let total = 0, avail = 0, swapTotal = 0, swapFree = 0;
                for (const line of lines) {
                    const [, key, val] = line.match(/^(\w+):\s+(\d+)/) ?? [];
                    if (key === "MemTotal") total = Number(val);
                    if (key === "MemAvailable") avail = Number(val);
                    if (key === "SwapTotal") swapTotal = Number(val);
                    if (key === "SwapFree") swapFree = Number(val);
                }
                if (total > 0) {
                    root.memPercent = Math.round(((total - avail) / total) * 100);
                    root.memUsedGB = (total - avail) / 1048576;
                    root.memTotalGB = total / 1048576;
                }
                root.swapUsedGB = (swapTotal - swapFree) / 1048576;
                root.swapTotalGB = swapTotal / 1048576;
            }
        }
    }

    Process {
        id: cpuFreqProc
        command: ["sh", "-c", "grep '^cpu MHz' /proc/cpuinfo | awk '{sum+=$4; n++} END {if (n>0) print sum/n/1000}'"]
        stdout: StdioCollector {
            onStreamFinished: root.cpuFreqGHz = Number(this.text.trim()) || 0
        }
    }

    Process {
        id: diskProc
        command: ["sh", "-c", "df -B1 --output=used,size,avail / | tail -1; df -h --output=used,size,avail / | tail -1"]
        stdout: StdioCollector {
            onStreamFinished: {
                const lines = this.text.trim().split("\n");
                const [used, size] = lines[0].trim().split(/\s+/).map(Number);
                const [usedH, sizeH, availH] = lines[1].trim().split(/\s+/);
                if (size > 0) root.diskPercent = Math.round((used / size) * 100);
                root.diskUsed = usedH ?? "";
                root.diskTotal = sizeH ?? "";
                root.diskFree = availH ?? "";
            }
        }
    }

    Process {
        id: tempProc
        // Prefer k10temp's Tctl (the real CPU die sensor, once hwmonProbe
        // below resolves it) over thermal_zone0 — thermal_zone0 is just
        // "whichever ACPI thermal zone sorts first" (this machine has 7 of
        // them, none of them k10temp), which happened to track Tctl within
        // ~4°C on this platform but isn't documented to and isn't the CPU
        // die reading TemperatureWidget.qml's tooltip claims it is. Falls
        // back to thermal_zone0 until the one-shot probe resolves, and
        // permanently on any machine without a k10temp node (Intel CPUs use
        // coretemp instead).
        command: root._cpuTempPath
            ? ["sh", "-c", `cat "${root._cpuTempPath}" 2>/dev/null || echo 0`]
            : ["sh", "-c", "cat /sys/class/thermal/thermal_zone0/temp 2>/dev/null || echo 0"]
        stdout: StdioCollector {
            onStreamFinished: root.tempC = Math.round(Number(this.text.trim()) / 1000)
        }
    }

    // One-shot at startup: locate the CPU die sensor, any GPU/NVMe temp
    // hwmon path, and any fan RPM input, so the polling below knows what's
    // available and where to read it from.
    Process {
        id: hwmonProbe
        running: true
        command: ["sh", "-c", `
            for h in /sys/class/hwmon/hwmon*; do
                name=$(cat "$h/name" 2>/dev/null)
                case "$name" in
                    k10temp|coretemp) [ -f "$h/temp1_input" ] && echo "CPU:$h/temp1_input" ;;
                    amdgpu) [ -f "$h/temp1_input" ] && echo "GPU:$h/temp1_input" ;;
                    nvme*) [ -f "$h/temp1_input" ] && echo "DISK:$h/temp1_input" ;;
                esac
                for f in "$h"/fan*_input; do
                    [ -f "$f" ] && echo "FAN:$f"
                done
            done
            true
        `]
        stdout: StdioCollector {
            onStreamFinished: {
                for (const line of this.text.trim().split("\n")) {
                    const [kind, path] = line.split(":");
                    if (kind === "CPU" && path) { root._cpuTempPath = path; }
                    else if (kind === "GPU" && path) { root._gpuTempPath = path; root.graphicsTempAvailable = true; }
                    else if (kind === "DISK" && path) { root._diskTempPath = path; root.diskTempAvailable = true; }
                    else if (kind === "FAN" && path) { root._fanPath = path; root.fanAvailable = true; }
                }
            }
        }
    }

    Timer {
        interval: 10000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: hwmonReadProc.running = true
    }

    Process {
        id: hwmonReadProc
        // `printf '%s\n' "$(cat path)"` always emits exactly one line per
        // field, even when the underlying path is /dev/null (empty command
        // substitution -> printf still emits the newline) — plain
        // `cat path; echo` doesn't: cat's own trailing newline plus the
        // extra echo produced a spurious blank line whenever a real file
        // was read (shifting every field after it left by one, e.g. the
        // disk temp field silently reading the fan's value instead), while
        // a /dev/null field produced zero bytes and no line at all
        // (collapsing the field instead of shifting). Either way the fixed
        // 3-line destructure below went out of alignment.
        command: root._gpuTempPath || root._diskTempPath || root._fanPath
            ? ["sh", "-c", `printf '%s\\n' "$(cat "${root._gpuTempPath || "/dev/null"}" 2>/dev/null)"; ` +
                `printf '%s\\n' "$(cat "${root._diskTempPath || "/dev/null"}" 2>/dev/null)"; ` +
                `printf '%s\\n' "$(cat "${root._fanPath || "/dev/null"}" 2>/dev/null)"`]
            : ["true"]
        stdout: StdioCollector {
            onStreamFinished: {
                const [gpu, disk, fan] = this.text.split("\n");
                if (root.graphicsTempAvailable) root.graphicsTempC = Math.round(Number(gpu) / 1000) || 0;
                if (root.diskTempAvailable) root.diskTempC = Math.round(Number(disk) / 1000) || 0;
                if (root.fanAvailable) root.fanRpm = parseInt(fan) || 0;
            }
        }
    }
}
