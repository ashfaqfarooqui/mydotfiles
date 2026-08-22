import QtQuick
import qs.theme

// Wraps a cluster of related widgets in a subtle rounded background so
// they read as one group, instead of every widget getting an individual
// separator — separators (see Separator.qml) go *between* pills, not
// inside them.
Item {
    id: root
    default property alias content: inner.data
    property int innerSpacing: 10
    property int padding: 6

    implicitWidth: inner.implicitWidth + padding * 2
    implicitHeight: inner.implicitHeight + padding
    width: implicitWidth
    height: implicitHeight

    Rectangle {
        anchors.fill: parent
        radius: 8
        color: Theme.surface1
        opacity: 0.35
    }

    Row {
        id: inner
        anchors.centerIn: parent
        spacing: root.innerSpacing
    }
}
