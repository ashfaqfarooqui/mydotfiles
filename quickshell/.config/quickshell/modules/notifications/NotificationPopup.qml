import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import Quickshell.Services.Notifications
import qs.theme
import qs.config
import qs.services

// Replaces swaync's toast popups (positionX: right, positionY: top,
// notification-window-width: 380). Stacks Notifications.popupQueue in one
// PanelWindow rather than one window per toast, simpler than the
// per-notification-window approach and avoids needing a Variants/model
// keyed by notification id just to manage window lifetimes.
Scope {
    id: root

    LazyLoader {
        active: Notifications.popupQueue.length > 0

        PanelWindow {
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors {
                top: true
                right: true
            }
            margins {
                top: 10
                right: 10
            }
            implicitWidth: Config.px(380)
            implicitHeight: column.implicitHeight
            color: "transparent"
            exclusiveZone: 0
            // A bare `mask: Region {}` (zero x/y/width/height, per Quickshell's
            // Region docs) makes the *entire* window click-through — this was
            // silently swallowing every click in this popup (dismiss X, action
            // buttons) from day one, not just after this file's redesign.
            // Binding the mask to the actual rendered Column makes only the
            // real toast content interactive instead of the whole transparent
            // window.
            mask: Region { item: column }

            Column {
                id: column
                width: parent.width
                spacing: 8

                // Column (a Positioner) only exposes add/move/populate
                // transitions — there is no `remove` hook, so an item
                // spliced straight out of Notifications.popupQueue would
                // vanish from the Repeater instantly with no chance to
                // animate. Entries are added to this locally-owned queue
                // immediately (so `add`'s slide-in still plays), and only
                // actually spliced out of it after their own fade-out
                // finishes, driven by each delegate's own Behavior below.
                property var displayQueue: []

                function sync() {
                    const liveIds = Notifications.popupQueue.map(n => n.id);
                    const known = column.displayQueue.map(d => d.id);
                    let next = column.displayQueue;
                    for (const n of Notifications.popupQueue) {
                        if (!known.includes(n.id)) next = [...next, { id: n.id, notification: n }];
                    }
                    column.displayQueue = next;
                }

                Connections {
                    target: Notifications
                    function onPopupQueueChanged_() { column.sync(); }
                }

                Component.onCompleted: column.sync()

                add: Transition {
                    ParallelAnimation {
                        NumberAnimation { property: "opacity"; from: 0; to: 1; duration: 200; easing.type: Easing.OutCubic }
                        NumberAnimation { property: "x"; from: column.width; to: 0; duration: 200; easing.type: Easing.OutCubic }
                    }
                }
                move: Transition {
                    NumberAnimation { property: "y"; duration: 150; easing.type: Easing.OutCubic }
                }

                Repeater {
                    model: column.displayQueue
                    delegate: Rectangle {
                        id: toast
                        required property var modelData
                        readonly property var notification: modelData.notification
                        readonly property bool stillLive: Notifications.popupQueue.some(n => n.id === modelData.id)
                        width: column.width
                        implicitHeight: content.implicitHeight + 24
                        radius: 10
                        color: Theme.surface0
                        opacity: stillLive ? 1 : 0

                        Behavior on opacity { NumberAnimation { duration: 150; easing.type: Easing.InCubic } }

                        onStillLiveChanged: if (!stillLive) removeTimer.start()

                        Timer {
                            id: removeTimer
                            interval: 160
                            onTriggered: column.displayQueue = column.displayQueue.filter(d => d.id !== modelData.id)
                        }

                        readonly property color urgencyAccent: notification.urgency === NotificationUrgency.Critical ? Theme.red
                            : notification.urgency === NotificationUrgency.Low ? Theme.sky
                            : Theme.blue

                        Rectangle {
                            width: 3
                            radius: 1.5
                            color: toast.urgencyAccent
                            anchors.top: parent.top
                            anchors.bottom: parent.bottom
                            anchors.left: parent.left
                            anchors.topMargin: 10
                            anchors.bottomMargin: 10
                            anchors.leftMargin: 4
                        }

                        ColumnLayout {
                            id: content
                            anchors.fill: parent
                            anchors.margins: 12
                            anchors.leftMargin: 18
                            spacing: 6

                            RowLayout {
                                spacing: 8

                                IconImage {
                                    readonly property string iconSource: notification.image !== "" ? notification.image
                                        : notification.appIcon !== "" ? Quickshell.iconPath(notification.appIcon, true) : ""
                                    visible: iconSource !== ""
                                    source: iconSource
                                    implicitSize: 24
                                    asynchronous: true
                                }

                                Text {
                                    text: notification.summary
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.bold: true
                                    font.pixelSize: Config.px(13)
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }

                                Text {
                                    text: "\u{F0156}"
                                    color: Theme.overlay0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(14)

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: toast.notification.dismiss()
                                    }
                                }
                            }

                            Text {
                                visible: notification.body !== ""
                                text: notification.body
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(12)
                                wrapMode: Text.WordWrap
                                Layout.fillWidth: true
                                maximumLineCount: 4
                                elide: Text.ElideRight
                            }

                            RowLayout {
                                visible: notification.actions.length > 0
                                spacing: 6

                                Repeater {
                                    model: notification.actions
                                    delegate: Rectangle {
                                        required property var modelData
                                        implicitWidth: actionText.implicitWidth + 16
                                        implicitHeight: Config.px(24)
                                        radius: 6
                                        color: Theme.surface1

                                        Text {
                                            id: actionText
                                            anchors.centerIn: parent
                                            text: modelData.text
                                            color: Theme.text
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(11)
                                        }

                                        MouseArea {
                                            anchors.fill: parent
                                            onClicked: modelData.invoke()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
