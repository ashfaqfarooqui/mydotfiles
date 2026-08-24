import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's clock#date module. The hover-calendar this used to carry
// moved to CalendarPanel.qml (opened by left-clicking ClockWidget.qml) so
// there's one calendar popup, not two — this is just the date readout.
Text {
    property date now: new Date()

    text: "󰨳 " + Qt.formatDateTime(now, "ddd MM-dd")
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    Timer {
        interval: 60000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: parent.now = new Date()
    }
}
