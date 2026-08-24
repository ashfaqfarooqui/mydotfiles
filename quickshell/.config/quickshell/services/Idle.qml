pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.services

// Replaces hypridle. Uses Quickshell.Wayland.IdleMonitor (ext-idle-notify-v1,
// confirmed present at /usr/lib/qt6/qml/Quickshell/Wayland/_IdleNotify on
// this system's quickshell 0.3.1) instead of a separate hypridle process.
// respectInhibitors mirrors hypridle's own honoring of idle-inhibit clients
// (fullscreen video, etc) automatically through the standard protocol,
// replacing IdleInhibit.qml's old pgrep/pkill-hypridle hack with a real
// stayAwake flag this monitor's `enabled` binds to instead.
//
// hypridle.conf had 4 independent timeout stages; IdleMonitor only exposes
// one timeout, so the 3 later stages are separate Timers chained off the
// first onIsIdleChanged, the same staggering approach basecamp/omarchy's
// shell/plugins/services/idle/Service.qml uses for its screensaver/lock split.
Singleton {
    id: root

    property bool stayAwake: false
    readonly property bool idleEnabled: !stayAwake

    // hypridle.conf's 4 listener timeouts, in seconds from idle-start.
    readonly property int backlightTimeout: 150
    readonly property int kbdBacklightTimeout: 150
    readonly property int lockTimeout: 300
    readonly property int dpmsTimeout: 430

    readonly property int baseTimeout: Math.min(backlightTimeout, kbdBacklightTimeout, lockTimeout, dpmsTimeout)

    function toggle() {
        stayAwake = !stayAwake;
    }

    function tooltipText() {
        return stayAwake ? "Screen won't sleep\nClick to enable idle" : "Idle mode active\nClick to inhibit";
    }

    IdleMonitor {
        id: monitor
        enabled: root.idleEnabled
        timeout: root.baseTimeout
        respectInhibitors: true
        onIsIdleChanged: {
            if (isIdle) root.startIdleCycle();
            else root.cancelIdleCycle();
        }
    }

    function startIdleCycle() {
        backlightTimer.restart();
        kbdBacklightTimer.restart();
        lockTimer.restart();
        dpmsTimer.restart();
    }

    // Mirrors hypridle.conf's on-resume commands, run once per stage that
    // actually fired (a Timer that never triggered has nothing to undo).
    function cancelIdleCycle() {
        if (!backlightTimer.running && backlightTimer.fired) Quickshell.execDetached(["brightnessctl", "-r"]);
        if (!kbdBacklightTimer.running && kbdBacklightTimer.fired) Quickshell.execDetached(["brightnessctl", "-rd", "rgb:kbd_backlight"]);
        if (!dpmsTimer.running && dpmsTimer.fired) Quickshell.execDetached(["hyprctl", "dispatch", "dpms", "on"]);
        backlightTimer.stop();
        kbdBacklightTimer.stop();
        lockTimer.stop();
        dpmsTimer.stop();
        backlightTimer.fired = false;
        kbdBacklightTimer.fired = false;
        dpmsTimer.fired = false;
    }

    Timer {
        id: backlightTimer
        interval: Math.max(0, root.backlightTimeout - root.baseTimeout) * 1000
        repeat: false
        property bool fired: false
        onTriggered: { fired = true; Quickshell.execDetached(["brightnessctl", "-s", "set", "20"]); }
    }

    Timer {
        id: kbdBacklightTimer
        interval: Math.max(0, root.kbdBacklightTimeout - root.baseTimeout) * 1000
        repeat: false
        property bool fired: false
        onTriggered: { fired = true; Quickshell.execDetached(["brightnessctl", "-sd", "rgb:kbd_backlight", "set", "0"]); }
    }

    Timer {
        id: lockTimer
        interval: Math.max(0, root.lockTimeout - root.baseTimeout) * 1000
        repeat: false
        onTriggered: Lock.beginLock()
    }

    Timer {
        id: dpmsTimer
        interval: Math.max(0, root.dpmsTimeout - root.baseTimeout) * 1000
        repeat: false
        property bool fired: false
        onTriggered: { fired = true; Quickshell.execDetached(["hyprctl", "dispatch", "dpms", "off"]); }
    }

    // hypridle.conf's before_sleep_cmd/after_sleep_cmd ran on logind's
    // PrepareForSleep signal. `gdbus monitor` subscribes as an ordinary
    // signal match (not eavesdropping), so it works unprivileged — plain
    // `busctl monitor`/`dbus-monitor --system` both need a polkit rule this
    // user doesn't have (confirmed: both fail with AccessDenied here).
    Process {
        id: sleepMonitor
        command: ["gdbus", "monitor", "--system", "--dest", "org.freedesktop.login1", "--object-path", "/org/freedesktop/login1"]
        running: true
        stdout: SplitParser {
            onRead: line => {
                if (!line.includes("PrepareForSleep")) return;
                if (line.includes("true")) {
                    Lock.beginLock();
                    Quickshell.execDetached(["hyprctl", "dispatch", "dpms", "off"]);
                } else if (line.includes("false")) {
                    Quickshell.execDetached(["hyprctl", "dispatch", "dpms", "on"]);
                }
            }
        }
        onExited: restartTimer.start()
    }

    Timer {
        id: restartTimer
        interval: 2000
        onTriggered: sleepMonitor.running = true
    }
}
