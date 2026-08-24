import QtQuick
import QtQuick.Window
import qs.theme
import qs.services

// Bar widget for Capture.qml's wf-recorder state. Click to stop — same
// affordance as clicking a running-process tray icon.
Item {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    visible: Capture.recording
    implicitWidth: dot.width
    implicitHeight: dot.height

    Rectangle {
        id: dot
        anchors.centerIn: parent
        width: 10
        height: 10
        radius: 5
        color: Theme.red

        SequentialAnimation on opacity {
            running: Capture.recording
            loops: Animation.Infinite
            NumberAnimation { from: 1.0; to: 0.35; duration: 700; easing.type: Easing.InOutQuad }
            NumberAnimation { from: 0.35; to: 1.0; duration: 700; easing.type: Easing.InOutQuad }
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: Capture.stopRecording()
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show("Recording — click to stop", point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
