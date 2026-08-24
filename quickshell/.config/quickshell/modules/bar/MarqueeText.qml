import QtQuick
import qs.theme
import qs.config
import qs.services

// Drop-in replacement for a width-capped, elided Text: renders normally
// (no clipping, no animation) while content fits within maxWidth, and only
// switches to a looping horizontal scroll once implicitWidth overflows it.
// The loop is gated by a `running:` binding tied to that overflow check
// (not a manual start/stop toggle) so it re-evaluates automatically as the
// text changes, and short/typical titles — the common case — never pay for
// a running animation.
Item {
    id: root

    property string text
    property color color: Theme.text
    property string fontFamily: Config.fontFamily
    property int fontWeight: Config.fontWeight
    property int pixelSize: Settings.fontSize
    property int maxWidth: 320
    property int gap: 24
    property real speed: 35 // px/sec

    readonly property bool overflowing: label.implicitWidth > root.maxWidth

    implicitWidth: Math.min(label.implicitWidth, root.maxWidth)
    implicitHeight: label.implicitHeight
    clip: true

    Row {
        id: track
        spacing: root.gap
        x: 0

        Text {
            id: label
            text: root.text
            color: root.color
            font.family: root.fontFamily
            font.pixelSize: root.pixelSize
            font.weight: root.fontWeight
        }

        Text {
            text: root.text
            color: root.color
            font.family: root.fontFamily
            font.pixelSize: root.pixelSize
            font.weight: root.fontWeight
            visible: root.overflowing
        }
    }

    SequentialAnimation {
        running: root.overflowing
        loops: Animation.Infinite

        PauseAnimation { duration: 1200 }
        NumberAnimation {
            target: track
            property: "x"
            to: -(label.implicitWidth + root.gap)
            duration: (label.implicitWidth + root.gap) / root.speed * 1000
            easing.type: Easing.Linear
        }
        PauseAnimation { duration: 600 }
        ScriptAction { script: track.x = 0 }
    }

    onOverflowingChanged: if (!overflowing) track.x = 0
}
