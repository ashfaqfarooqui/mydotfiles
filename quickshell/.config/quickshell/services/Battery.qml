pragma Singleton
import Quickshell
import Quickshell.Services.UPower

// Replaces waybar's "battery" module (states/icons ported in BatteryWidget.qml).
Singleton {
    readonly property var device: UPower.displayDevice
    // UPowerDevice.percentage is a 0.0-1.0 fraction here, not 0-100
    // (confirmed empirically: a near-full battery showed as "1%" before
    // this fix), unlike the D-Bus spec's raw 0-100 value.
    readonly property real percent: (device?.percentage ?? 0) * 100
    readonly property real changeRate: device?.changeRate ?? 0
    readonly property bool charging: changeRate > 0
    readonly property bool onBattery: UPower.onBattery
    readonly property real timeToEmpty: device?.timeToEmpty ?? 0
    readonly property real timeToFull: device?.timeToFull ?? 0
}
