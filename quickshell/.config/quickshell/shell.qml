import Quickshell
import qs.modules.bar
import qs.modules.osd
import qs.modules.notifications
import qs.modules.launcher

// Phase 1: full-parity bar (one per monitor, full/reduced split by output
// name) + volume/brightness OSD popups.
// Phase 2: notification toasts + control center (replaces swaync — not yet
// cut over live, see services/Notifications.qml).
// Phase 3: launcher/switcher/clipboard/cheatsheet/theme-picker/power-menu —
// each visibility-gated via services/LauncherBus.qml, off by default; no
// keybind repointed to them yet (still rofi/wlogout live, see the plan).
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
    PowerMenuUI {}
}
