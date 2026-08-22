import Quickshell
import qs.modules.bar
import qs.modules.osd
import qs.modules.notifications

// Phase 1: full-parity bar (one per monitor, full/reduced split by output
// name) + volume/brightness OSD popups.
// Phase 2: notification toasts + control center (replaces swaync — not yet
// cut over live, see services/Notifications.qml).
ShellRoot {
    Variants {
        model: Quickshell.screens
        Bar {}
    }

    VolumeOSD {}
    BrightnessOSD {}
    NotificationPopup {}
    ControlCenter { id: controlCenter }
}
