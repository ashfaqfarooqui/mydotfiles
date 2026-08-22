pragma Singleton
import Quickshell
import Quickshell.Services.SystemTray

// Replaces waybar's "tray" module.
Singleton {
    readonly property var items: SystemTray.items
}
