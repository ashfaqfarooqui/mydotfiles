import QtQuick
import qs.theme
import qs.config

// Replaces waybar's clock#date module. The original's full calendar-grid
// tooltip (year view, scroll-to-change-month) is deferred — this is a
// same-format label only for now; the calendar popup is a Phase 1 polish
// follow-up, not core functional parity.
Text {
    property date now: new Date()

    text: "󰨳 " + Qt.formatDateTime(now, "ddd MM-dd")
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    Timer {
        interval: 60000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: parent.now = new Date()
    }
}
