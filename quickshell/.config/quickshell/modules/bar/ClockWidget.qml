import QtQuick
import qs.theme
import qs.config

// Replaces waybar's clock#time module.
Text {
    property bool altFormat: false

    text: "󱑂 " + Qt.formatDateTime(now, altFormat ? "hh:mm AP" : "HH:mm")
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    property date now: new Date()
    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: parent.now = new Date()
    }

    MouseArea {
        anchors.fill: parent
        onClicked: parent.altFormat = !parent.altFormat
    }
}
