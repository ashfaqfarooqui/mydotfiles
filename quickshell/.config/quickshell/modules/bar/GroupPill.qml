import QtQuick
import QtQuick.Layouts
import qs.theme

// Wraps a cluster of related widgets in a subtle rounded background so
// they read as one group, instead of every widget getting an individual
// separator — separators (see Separator.qml) go *between* pills, not
// inside them.
//
// Uses RowLayout (not a plain Row) so mixed-height children — e.g.
// NetSpeedWidget's two-line Column next to NetworkWidget's one-line Text —
// are vertically centered against each other. A plain Row only manages the
// x-axis and leaves each child's y at its own default (0/top), which made
// shorter siblings look pinned to the top of a taller neighbor instead of
// centered — reported as the network/wifi row looking "skewed"/two-column.
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

    RowLayout {
        id: inner
        anchors.centerIn: parent
        spacing: root.innerSpacing
    }
}
