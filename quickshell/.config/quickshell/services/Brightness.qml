pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs.services

// Replaces waybar's custom/backlight module + brightness-control.sh.
// No first-party brightness service exists in Quickshell 0.3.1, so this
// wraps brightnessctl the same way the original bash script did.
Singleton {
    id: root
    property int percent: 50

    // Visibility for BrightnessPanel.qml (modules/bar/) — same pattern as
    // Network.qml/Bluetooth.qml/Battery.qml, see Network.qml for why the
    // per-screen gating is mandatory.
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            Network.hidePanel();
            Bluetooth.hidePanel();
            Battery.hidePanel();
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

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: poll.running = true
    }

    Process {
        id: poll
        command: ["brightnessctl", "-m"]
        stdout: StdioCollector {
            onStreamFinished: {
                // format: device,class,current,percent%,max
                const fields = this.text.trim().split(",");
                if (fields.length >= 4) root.percent = parseInt(fields[3]);
            }
        }
    }

    function set(pct) {
        const clamped = Math.max(1, Math.min(100, pct));
        Quickshell.execDetached(["brightnessctl", "set", clamped + "%"]);
        root.percent = clamped;
    }

    function step(delta) {
        set(percent + delta);
    }
}
