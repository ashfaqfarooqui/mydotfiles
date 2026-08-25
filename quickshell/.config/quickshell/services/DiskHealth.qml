pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// NVMe SMART health via udisks2's D-Bus API. Deliberately NOT smartctl
// against the raw device node — that needs root (confirmed: `smartctl -a
// /dev/nvme0n1` as a normal user -> Permission denied), which would mean
// either a new polkit rule scoped to smartctl or enabling smartd.service and
// reading a status file it writes. udisks2 already coldplugs this exact
// data over D-Bus with no root and no polkit prompt for a local active
// session (confirmed live: calling its SmartUpdate method as a normal user
// succeeded with no auth dialog) — this is the same tradeoff Gpu.qml/
// SystemStats.qml already made preferring sysfs over needing a privileged
// helper.
//
// The block device's kernel name (nvme0n1) is resolved once at startup by
// scanning /sys/block for the first whole-disk NVMe device (not a
// partition), then that block device's associated drive object path is
// looked up via D-Bus — the drive path itself is keyed on the drive's model
// + serial, so this isn't hardcoded to one specific drive/machine.
Singleton {
    id: root

    property bool available: false
    property bool healthy: true
    property var warnings: [] // e.g. ["available_spare", "temperature"]
    property int powerOnHours: -1
    property string selftestStatus: ""
    property string _drivePath: ""

    Process {
        id: resolveProc
        running: true
        command: ["sh", "-c", `
            dev=$(ls /sys/block 2>/dev/null | grep -E '^nvme[0-9]+n[0-9]+$' | head -1)
            [ -z "$dev" ] && exit 0
            busctl get-property org.freedesktop.UDisks2 \
                "/org/freedesktop/UDisks2/block_devices/$dev" \
                org.freedesktop.UDisks2.Block Drive 2>/dev/null
        `]
        stdout: StdioCollector {
            onStreamFinished: {
                // busctl prints object-path properties as: o "/path/here"
                const m = /o\s+"([^"]+)"/.exec(this.text);
                if (m) root._drivePath = m[1];
            }
        }
    }

    // SMART data is cached/coldplugged by udisks2, not read fresh on every
    // poll — a minute is plenty and matches this shell's other slow-moving
    // hwmon polls (SystemStats.qml's temp/hwmon Timer is 10s, this is
    // slower since power-on-hours/warnings change far less often than temp).
    Timer {
        interval: 60000
        running: root._drivePath !== ""
        repeat: true
        triggeredOnStart: true
        onTriggered: pollProc.running = true
    }

    Process {
        id: pollProc
        command: root._drivePath
            ? ["sh", "-c", `
                busctl get-property org.freedesktop.UDisks2 "${root._drivePath}" org.freedesktop.UDisks2.NVMe.Controller SmartCriticalWarning 2>/dev/null
                busctl get-property org.freedesktop.UDisks2 "${root._drivePath}" org.freedesktop.UDisks2.NVMe.Controller SmartPowerOnHours 2>/dev/null
                busctl get-property org.freedesktop.UDisks2 "${root._drivePath}" org.freedesktop.UDisks2.NVMe.Controller SmartSelftestStatus 2>/dev/null
            `]
            : ["true"]
        stdout: StdioCollector {
            onStreamFinished: {
                const lines = this.text.split("\n");

                // "as N <elem> <elem> ..." — N is the element COUNT, not a
                // value (busctl's array-property format), so an empty
                // array ("as 0") means zero warning flags set, i.e. healthy.
                const warnLine = lines[0] || "";
                const warnMatches = [...warnLine.matchAll(/"([^"]*)"/g)];
                const hoursMatch = /t\s+(\d+)/.exec(lines[1] || "");
                const statusMatch = /s\s+"([^"]*)"/.exec(lines[2] || "");

                root.available = warnLine.startsWith("as") && hoursMatch !== null;
                if (!root.available) return;
                root.warnings = warnMatches.map(m => m[1]);
                root.healthy = root.warnings.length === 0;
                root.powerOnHours = hoursMatch ? Number(hoursMatch[1]) : -1;
                root.selftestStatus = statusMatch ? statusMatch[1] : "";
            }
        }
    }
}
