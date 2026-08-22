import QtQuick
import qs.services

// Replaces waybar's "tray" module.
Row {
    spacing: 6

    Repeater {
        model: Tray.items.values
        delegate: Image {
            required property var modelData
            source: modelData.icon
            width: 16
            height: 16
            sourceSize: Qt.size(16, 16)

            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.LeftButton | Qt.RightButton
                onClicked: mouse => {
                    if (mouse.button === Qt.LeftButton) modelData.activate();
                    else modelData.secondaryActivate();
                }
            }
        }
    }
}
