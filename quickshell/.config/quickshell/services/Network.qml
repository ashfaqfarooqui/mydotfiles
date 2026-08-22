pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Networking as QN

// Replaces waybar's "network" module. Uses the native Quickshell.Networking
// binding (confirmed via https://quickshell.org/docs/v0.3.0/types/Quickshell.Networking/
// — Networking.devices is a live ObjectModel<NetworkDevice>, NetworkDevice.connected
// is a live bool, and WifiNetwork.signalStrength/Network.name update live) instead
// of polling nmcli on a Timer.
Singleton {
    id: root

    readonly property var activeDevice: QN.Networking.devices.values.find(d => d.connected) ?? null

    readonly property string kind: activeDevice === null
        ? "disconnected"
        : (activeDevice.type === QN.DeviceType.Wifi ? "wifi" : "ethernet")

    readonly property var activeWifiNetwork: (activeDevice !== null && activeDevice.type === QN.DeviceType.Wifi)
        ? activeDevice.networks.values.find(n => n.connected) ?? null
        : null

    readonly property string ssid: activeWifiNetwork?.name ?? ""
    // WifiNetwork.signalStrength is 0.0-1.0; keep signalStrength as a 0-100
    // int to match the previous nmcli-based contract bar widgets already use.
    readonly property int signalStrength: Math.round((activeWifiNetwork?.signalStrength ?? 0) * 100)
}
