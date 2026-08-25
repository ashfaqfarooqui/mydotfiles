import QtQuick
import Quickshell
import Quickshell.Io
import qs.services

// External entry point for Hyprland keybinds (Phase 3 cutover): each
// Hyprland-bound key runs `quickshell ipc call <target> <function>` via
// hl.dsp.exec_cmd, since a keybind fires in an external process and has no
// direct handle into the running QML tree. IpcHandler is Quickshell's
// built-in bridge for exactly this — confirmed via community usage
// (e.g. darth-malu/quickshell:IpcHandler.qml, liixini/skwd-wall:shell.qml)
// since quickshell.org's own docs site was unreachable at write time. Every
// handler here just forwards to the existing LauncherBus/Notifications
// toggle functions, so the popups themselves are unchanged.
Item {
    IpcHandler {
        target: "launcher"
        function toggle(): void { LauncherBus.toggleLauncher(); }
    }

    IpcHandler {
        target: "windowSwitcher"
        function toggle(): void { LauncherBus.toggleWindowSwitcher(); }
    }

    IpcHandler {
        target: "clipboard"
        function toggle(): void { LauncherBus.toggleClipboard(); }
    }

    IpcHandler {
        target: "cheatsheet"
        function toggle(): void { LauncherBus.toggleCheatsheet(); }
    }

    IpcHandler {
        target: "themePicker"
        function toggle(): void { LauncherBus.toggleThemePicker(); }
    }

    IpcHandler {
        target: "lockAppearance"
        function toggle(): void { LauncherBus.toggleLockAppearance(); }
    }

    IpcHandler {
        target: "emoji"
        function toggle(): void { LauncherBus.toggleEmoji(); }
    }

    // Registered now so the target exists ahead of time, but no keybind
    // calls this yet — SUPER+Escape still runs the old Session submap
    // until the user live-tests Lock/Suspend/Hibernate/Reboot/Poweroff/
    // Logout themselves (see the plan's PowerMenuUI incident writeup).
    IpcHandler {
        target: "powerMenu"
        function toggle(): void { LauncherBus.togglePowerMenu(); }
    }

    IpcHandler {
        target: "notifications"
        function toggle(): void { Notifications.toggleControlCenter(); }
    }

    // Network/Bluetooth panels are primarily opened by clicking their bar
    // widget (which already knows its own Screen.name for the mandatory
    // per-monitor gating — see Network.qml/Bluetooth.qml), so these IPC
    // targets default to the primary screen for the rare external-trigger
    // case, same as every other handler here forwarding to a bus function.
    IpcHandler {
        target: "network"
        function toggle(): void { Network.togglePanel(Quickshell.screens[0]?.name ?? ""); }
    }

    IpcHandler {
        target: "bluetooth"
        function toggle(): void { Bluetooth.togglePanel(Quickshell.screens[0]?.name ?? ""); }
    }

    IpcHandler {
        target: "calendar"
        function toggle(): void { Calendar.togglePanel(Quickshell.screens[0]?.name ?? ""); }
    }

    IpcHandler {
        target: "tailscale"
        function toggle(): void { Tailscale.togglePanel(Quickshell.screens[0]?.name ?? ""); }
        function connectToggle(): void { Tailscale.toggle(); }
    }

    IpcHandler {
        target: "weather"
        function toggle(): void { Weather.togglePanel(Quickshell.screens[0]?.name ?? ""); }
    }

    IpcHandler {
        target: "capture"
        function toggle(): void { Capture.toggleMenu(); }
    }

    // Lock/Idle are pragma Singleton (see services/Lock.qml, services/Idle.qml)
    // and only get instantiated the first time something touches them — that
    // first touch happens right here, exactly when a keybind or the idle
    // timer actually needs them.
    IpcHandler {
        target: "lock"
        function lock(): void { Lock.beginLock(); }
        function isLocked(): string { return Lock.locked ? "true" : "false"; }
    }

    IpcHandler {
        target: "idle"
        function enable(): void { Idle.stayAwake = false; }
        function disable(): void { Idle.stayAwake = true; }
        function toggle(): void { Idle.toggle(); }
    }
}
