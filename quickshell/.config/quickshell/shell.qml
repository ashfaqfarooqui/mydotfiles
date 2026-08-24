import Quickshell
import qs.modules.bar
import qs.modules.osd
import qs.modules.notifications
import qs.modules.launcher
import qs.modules.capture
import qs.modules.polkit
import qs.services

// Phase 1: full-parity bar (one per monitor, full/reduced split by output
// name) + volume/brightness OSD popups.
// Phase 2: notification toasts + control center (cut over live — swaync is
// stopped/disabled, see the plan's boot-cutover section).
// Phase 3: launcher/switcher/clipboard/cheatsheet/theme-picker/power-menu.
// Keybind cutover (see services/Ipc.qml + the plan's dated section): every
// popup except PowerMenuUI is now reachable from a real Hyprland keybind
// via `quickshell ipc call`. PowerMenuUI's IPC target is registered but
// nothing calls it yet — SUPER+Escape still runs the old Session submap
// until the user live-tests its destructive actions themselves.
ShellRoot {
    Variants {
        model: Quickshell.screens
        Bar {}
    }

    VolumeOSD {}
    BrightnessOSD {}
    NotificationPopup {}
    ControlCenter { id: controlCenter }

    Launcher {}
    WindowSwitcher {}
    ClipboardPicker {}
    Cheatsheet {}
    ThemePicker {}
    LockAppearancePicker {}
    EmojiPicker {}
    PowerMenuUI {}
    CaptureMenu {}
    PolkitDialog {}

    Ipc {}
}
