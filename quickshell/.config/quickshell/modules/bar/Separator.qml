import QtQuick
import qs.theme

// Thin vertical divider between bar sections — waybar's GTK box spacing
// gave modules implicit visual grouping via padding alone; this adds an
// explicit divider since a bare QML Row has no equivalent.
Rectangle {
    width: 1
    height: 18
    color: Theme.overlay0
    opacity: 0.4
    anchors.verticalCenter: parent.verticalCenter
}
