pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Bluetooth as QB
import qs.services

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

    // Visibility for BluetoothPanel.qml (modules/network/) — same pattern
    // as Network.qml's panelVisible/panelScreenName, see there for why the
    // per-screen gating is mandatory.
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            // All top-right panels anchor to the same bar position, so
            // leaving another one open makes it look like this click just
            // swapped its content instead of opening a new popup.
            Network.hidePanel();
            Battery.hidePanel();
            Brightness.hidePanel();
            Audio.hidePanel();
            Calendar.hidePanel();
            SystemStats.hidePanel();
            AgentsUsage.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }
}
