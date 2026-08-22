pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Bluetooth as QB

// Replaces waybar's "bluetooth" module. Uses the native Quickshell.Bluetooth
// binding for everything now, including the connected-device count —
// confirmed via https://quickshell.org/docs/v0.3.0/types/Quickshell.Bluetooth/
// that Bluetooth.devices is already "a list of all connected bluetooth
// devices across all adapters" (a live ObjectModel), so no bluetoothctl
// polling is needed at all.
Singleton {
    id: root
    readonly property bool enabled: QB.Bluetooth.defaultAdapter?.enabled ?? false
    readonly property int connectedCount: QB.Bluetooth.devices.values.length
}
