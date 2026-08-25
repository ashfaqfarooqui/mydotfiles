import QtQuick
import qs.theme

// Native 3x3 dot-grid rendering of the Tailscale mark, ported from
// basecamp/omarchy's shell/plugins/panels/tailscale/TailscaleIcon.qml — avoids
// tiny-SVG rendering quirks in the bar while keeping the official silhouette.
Item {
    id: root
    property real iconSize: 14
    property color tint: Theme.text

    implicitWidth: iconSize
    implicitHeight: iconSize

    readonly property real dotSize: Math.max(2, iconSize * 0.24)
    readonly property real mid: (iconSize - dotSize) / 2
    readonly property real end: iconSize - dotSize

    Dot { x: 0; y: 0; opacity: 0.24 }
    Dot { x: root.mid; y: 0; opacity: 0.24 }
    Dot { x: root.end; y: 0; opacity: 0.24 }
    Dot { x: 0; y: root.mid; opacity: 1.0 }
    Dot { x: root.mid; y: root.mid; opacity: 1.0 }
    Dot { x: root.end; y: root.mid; opacity: 1.0 }
    Dot { x: 0; y: root.end; opacity: 0.24 }
    Dot { x: root.mid; y: root.end; opacity: 1.0 }
    Dot { x: root.end; y: root.end; opacity: 0.24 }

    component Dot: Rectangle {
        width: root.dotSize
        height: root.dotSize
        radius: width / 2
        color: root.tint
    }
}
