import QtQuick
import QtQuick.Layouts
import Quickshell
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
            implicitWidth: 380
            implicitHeight: column.implicitHeight
            color: "transparent"
            exclusiveZone: 0
            mask: Region {}

            Column {
                id: column
                width: parent.width
                spacing: 8

                Repeater {
                    model: Notifications.popupQueue
                    delegate: Rectangle {
                        id: toast
                        required property var modelData
                        width: column.width
                        implicitHeight: content.implicitHeight + 24
                        radius: 10
                        color: Theme.surface0
                        border.color: modelData.urgency === NotificationUrgency.Critical ? Theme.red : "transparent"
                        border.width: modelData.urgency === NotificationUrgency.Critical ? 1 : 0

                        ColumnLayout {
                            id: content
                            anchors.fill: parent
                            anchors.margins: 12
                            spacing: 6

                            RowLayout {
                                spacing: 8

                                Image {
                                    visible: modelData.image !== "" || modelData.appIcon !== ""
                                    source: modelData.image !== "" ? modelData.image : modelData.appIcon
                                    Layout.preferredWidth: 24
                                    Layout.preferredHeight: 24
                                }

                                Text {
                                    text: modelData.summary
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.bold: true
                                    font.pixelSize: 13
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }

                                Text {
                                    text: "\u{F0156}"
                                    color: Theme.overlay0
                                    font.family: Config.fontFamily
                                    font.pixelSize: 14

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: toast.modelData.dismiss()
                                    }
                                }
                            }

                            Text {
                                visible: modelData.body !== ""
                                text: modelData.body
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: 12
                                wrapMode: Text.WordWrap
                                Layout.fillWidth: true
                                maximumLineCount: 4
                                elide: Text.ElideRight
                            }

                            RowLayout {
                                visible: modelData.actions.length > 0
                                spacing: 6

                                Repeater {
                                    model: modelData.actions
                                    delegate: Rectangle {
                                        required property var modelData
                                        implicitWidth: actionText.implicitWidth + 16
                                        implicitHeight: 24
                                        radius: 6
                                        color: Theme.surface1

                                        Text {
                                            id: actionText
                                            anchors.centerIn: parent
                                            text: modelData.text
                                            color: Theme.text
                                            font.family: Config.fontFamily
                                            font.pixelSize: 11
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
