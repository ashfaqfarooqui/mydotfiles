import QtQuick
import Quickshell.Widgets
import qs.services

// Replaces waybar's "tray" module.
Row {
    spacing: 6

    Repeater {
        model: Tray.items.values
        delegate: IconImage {
            required property var modelData
            // SystemTrayItem.icon is documented as "usable as an Image
            // source" directly (https://quickshell.org/docs/v0.1.0/types/
            // Quickshell.Services.SystemTray/SystemTrayItem/) — Quickshell
            // resolves it internally (including generating an image
            // provider URL for pixmap-only items like Nextcloud's tray
            // icon, which has no IconName). Running it through
            // Quickshell.iconPath() as if it were a bare theme name (the
            // DesktopEntry.icon pattern used in Launcher.qml) breaks those
            // pixmap-only items, since iconPath() can't resolve them and
            // returns "" — that emptied the entire tray.
            source: modelData.icon
            implicitSize: 16
            asynchronous: true

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
