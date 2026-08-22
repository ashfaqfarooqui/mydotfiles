import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/notification module. Now reads live state
// directly from services/Notifications.qml (Phase 2's native
// NotificationServer) instead of shelling to swaync-client -swb.
Text {
    text: ""
    color: Notifications.unreadCount > 0 ? Theme.red : (Notifications.dndEnabled ? Theme.overlay0 : Theme.text)
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Notifications.toggleControlCenter();
            else Notifications.toggleDnd();
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            Notifications.unreadCount + " notification" + (Notifications.unreadCount === 1 ? "" : "s") +
            (Notifications.dndEnabled ? " (DND on)" : "")
        ) : TooltipBus.hide()
    }
}
