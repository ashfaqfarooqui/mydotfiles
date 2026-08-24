import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's clock#time module. Format-cycling + seconds-aware tick
// rate modeled on basecamp/omarchy's quattro-branch clock BarWidget.qml.
Text {
    id: root

    // Small preset ring, cycled by right-click and persisted via
    // Settings.clockFormat so the choice survives a shell restart.
    readonly property var formats: [
        "dddd HH:mm",
        "HH:mm",
        "hh:mm AP",
        "HH:mm:ss",
        "yyyy-MM-dd HH:mm",
        "ddd, MMM d",
    ]

    text: "󱑂 " + Qt.formatDateTime(now, Settings.clockFormat)
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    property date now: new Date()

    // Only tick every second when the active format actually shows seconds —
    // otherwise tick once a minute, saving a repaint 59 times out of 60.
    readonly property bool needsSeconds: Settings.clockFormat.indexOf("s") !== -1

    Timer {
        interval: root.needsSeconds ? 1000 : 60000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.now = new Date()
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) {
                Calendar.togglePanel(Screen.name);
            } else {
                const idx = root.formats.indexOf(Settings.clockFormat);
                Settings.clockFormat = root.formats[(idx + 1) % root.formats.length];
            }
        }
    }
}
