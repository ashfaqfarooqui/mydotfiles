pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.UPower
import qs.services

// Replaces waybar's "battery" module (states/icons ported in BatteryWidget.qml).
Singleton {
    id: root

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

    // "Xh Ym", empty when the device isn't reporting a usable estimate
    // (UPowerDevice.timeToEmpty/timeToFull are 0 while the other one applies).
    function _formatSeconds(seconds) {
        if (!seconds || seconds <= 0) return "";
        const totalMinutes = Math.round(seconds / 60);
        const h = Math.floor(totalMinutes / 60);
        const m = totalMinutes % 60;
        if (h <= 0) return m + "m";
        return h + "h " + m + "m";
    }
    readonly property string timeToEmptyFormatted: _formatSeconds(timeToEmpty)
    readonly property string timeToFullFormatted: _formatSeconds(timeToFull)

    // healthPercentage is only meaningful when healthSupported is true
    // (confirmed via https://quickshell.org/docs/v0.3.0/types/Quickshell.Services.UPower/UPowerDevice/).
    readonly property bool healthSupported: device?.healthSupported ?? false
    readonly property real healthPercent: healthSupported ? (device?.healthPercentage ?? 0) : 0

    readonly property real powerDrawWatts: Math.abs(changeRate)

    // Single source of truth for the battery glyph — BatteryWidget.qml and
    // BatteryPanel.qml used to keep their own separate icon tables, and they
    // drifted (the panel's old 100%-charge codepoint was a partial-battery
    // glyph, rendering full charge as if it were half charged).
    readonly property var _levelIcons: ["󰁼", "󰁽", "󰁾", "󰁿", "󰂀", "󰂁", "󰂂"]
    function iconFor(pct, charging) {
        if (pct >= 100) return "󱃌";
        if (charging) return "󱘖";
        if (pct <= 15) return "󱃍";
        if (pct <= 30) return "󰁻";
        const idx = Math.min(_levelIcons.length - 1, Math.floor(pct / (100 / _levelIcons.length)));
        return _levelIcons[idx];
    }

    // energyCapacity is in watt-hours already (confirmed against
    // https://quickshell.org/docs/v0.3.0/types/Quickshell.Services.UPower/UPowerDevice/,
    // same doc that gives percent = energy / energyCapacity).
    readonly property real capacityWh: device?.energyCapacity ?? 0

    // Cycle count and charge-control thresholds aren't exposed by UPower at
    // all (confirmed against the UPowerDevice docs above) — basecamp/omarchy's
    // own battery panel (bin/omarchy-battery-status) reads them straight out
    // of sysfs the same way, so this does too.
    property int cycleCount: 0
    property int chargeThresholdStart: 0
    property int chargeThresholdEnd: 0
    readonly property bool chargeThresholdSet: chargeThresholdEnd > 0 && chargeThresholdEnd < 100

    Timer {
        interval: 15000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: sysfsProc.running = true
    }

    Process {
        id: sysfsProc
        command: ["sh", "-c",
            'cat /sys/class/power_supply/BAT*/cycle_count 2>/dev/null | head -1; echo; ' +
            'cat /sys/class/power_supply/BAT*/charge_control_start_threshold 2>/dev/null | head -1; echo; ' +
            'cat /sys/class/power_supply/BAT*/charge_control_end_threshold 2>/dev/null | head -1']
        stdout: StdioCollector {
            onStreamFinished: {
                const [cycles, start, end] = this.text.split("\n");
                root.cycleCount = parseInt(cycles) || 0;
                root.chargeThresholdStart = parseInt(start) || 0;
                root.chargeThresholdEnd = parseInt(end) || 0;
            }
        }
    }

    // UPowerDeviceState members confirmed against
    // https://quickshell.org/docs/v0.3.0/types/Quickshell.Services.UPower/UPowerDeviceState/ —
    // PendingCharge/PendingDischarge/Unknown/Empty all read as "Not charging".
    readonly property string stateText: {
        switch (device?.state) {
        case UPowerDeviceState.Charging: return "Charging";
        case UPowerDeviceState.Discharging: return "Discharging";
        case UPowerDeviceState.FullyCharged: return "Full";
        default: return "Not charging";
        }
    }

    // "Holding" — not a real UPower state, just a friendlier label for
    // "not charging, but pinned at the charge-limit ceiling" (the BMS is
    // deliberately keeping it there rather than fully charging further).
    readonly property string displayState: charging ? "Charging"
        : (chargeThresholdSet && percent >= chargeThresholdEnd - 2 ? "Holding" : stateText)

    // PowerProfiles.profile (Quickshell.Services.UPower) is a directly
    // settable property backing power-profiles-daemon, not a method call —
    // confirmed via https://quickshell.org/docs/v0.3.0/types/Quickshell.Services.UPower/PowerProfiles/.
    function setPowerProfile(profile) {
        PowerProfiles.profile = profile;
    }

    // Visibility for BatteryPanel.qml (modules/bar/) — same
    // toggle-flag-on-the-singleton pattern as Network.qml/Bluetooth.qml, see
    // Network.qml for why the per-screen gating is mandatory.
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            // All top-right panels share the same anchor position, so
            // leaving another one open makes it look like this click just
            // swapped its content instead of opening a new popup.
            Network.hidePanel();
            Bluetooth.hidePanel();
            Brightness.hidePanel();
            Audio.hidePanel();
            Calendar.hidePanel();
            SystemStats.hidePanel();
            AgentsUsage.hidePanel();
            Tailscale.hidePanel();
            Weather.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }
}
