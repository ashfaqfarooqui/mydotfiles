import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/notification module. Now reads live state
// directly from services/Notifications.qml (Phase 2's native
// NotificationServer) instead of shelling to swaync-client -swb.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    text: ""
    color: Notifications.unreadCount > 0 ? Theme.red : (Notifications.dndEnabled ? Theme.overlay0 : Theme.text)
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Notifications.toggleControlCenter();
            else Notifications.toggleDnd();
        }
    }

    // Augments (doesn't replace) the bell's own color signal above — the
    // color still needs to carry the DND-vs-unread-vs-clear three-state
    // distinction (a DND-muted bell with zero unread has no count to show,
    // but still needs to look visually "off"), which a count bubble alone
    // can't convey.
    Rectangle {
        visible: Notifications.unreadCount > 0
        width: Math.max(14, countText.implicitWidth + 6)
        height: 14
        radius: 7
        color: Theme.red
        anchors.top: parent.top
        anchors.right: parent.right
        anchors.topMargin: -4
        anchors.rightMargin: -6

        Text {
            id: countText
            anchors.centerIn: parent
            text: Notifications.unreadCount > 99 ? "99+" : String(Notifications.unreadCount)
            color: Theme.base
            font.family: Config.fontFamily
            font.pixelSize: Config.px(9)
            font.bold: true
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            Notifications.unreadCount + " notification" + (Notifications.unreadCount === 1 ? "" : "s") +
            (Notifications.dndEnabled ? " (DND on)" : "")
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
