import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire
import qs.theme
import qs.config
import qs.services

// Replaces swaync's control-center panel (swaync/.config/swaync/config.json:
// control-center-width 400, height 850, margins top/bottom 10, right 10,
// left 0, anchored top-right). Icon codepoints below are \u{XXXXX} escapes
// (outside the BMP, so \uXXXX 4-hex form doesn't cover them) extracted
// byte-exact from that same config.json and cross-checked against the
// installed JetBrainsMono Nerd Font's cmap — see Phase 1's icon-drop
// postmortem for why these are never hand-pasted.
Scope {
    id: root

    LazyLoader {
        active: Notifications.controlCenterVisible

        PanelWindow {
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors {
                top: true
                right: true
                bottom: true
            }
            margins {
                top: 10
                right: 10
                bottom: 10
            }
            implicitWidth: 400
            color: "transparent"
            exclusiveZone: 0
            // Keyboard focus / click-outside-to-close aren't wired yet —
            // closing via the bar's notification-badge toggle is sufficient
            // for this phase; revisit once the launcher work (Phase 3)
            // establishes a shared pattern for dismissable popups.

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 12

                    // --- Title bar ---
                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "Notification Center"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: 15
                            Layout.fillWidth: true
                        }

                        Text {
                            text: "\u{F01B4}"
                            color: Theme.subtext0
                            font.family: Config.fontFamily
                            font.pixelSize: 16

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Notifications.clearAll()
                            }
                        }
                    }

                    // --- 4-button grid ---
                    GridLayout {
                        Layout.fillWidth: true
                        columns: 4
                        columnSpacing: 8
                        rowSpacing: 8

                        component GridButton: Rectangle {
                            property string icon
                            property var onActivated
                            Layout.fillWidth: true
                            Layout.preferredHeight: 44
                            radius: 8
                            color: Theme.surface1

                            Text {
                                anchors.centerIn: parent
                                text: parent.icon
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: 16
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: parent.onActivated()
                            }
                        }

                        GridButton {
                            icon: "\u{F0373}"
                            onActivated: Quickshell.execDetached(["hyprlock"])
                        }
                        GridButton {
                            icon: "\u{F0E51}"
                            onActivated: Quickshell.execDetached(["sh", "-c", 'grim -g "$(slurp)" - | wl-copy'])
                        }
                        GridButton {
                            icon: "\u{F0DC8}"
                            onActivated: Quickshell.execDetached(["sh", "-c", 'grim "$(xdg-user-dir PICTURES)/Screenshots/screenshot-$(date +%Y%m%d-%H%M%S).png"'])
                        }
                        GridButton {
                            icon: "\u{F0474}"
                            onActivated: Quickshell.execDetached(["wlogout"])
                        }
                    }

                    // --- DND toggle ---
                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "Do Not Disturb"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: 13
                            Layout.fillWidth: true
                        }

                        Rectangle {
                            width: 40
                            height: 22
                            radius: 11
                            color: Notifications.dndEnabled ? Theme.blue : Theme.surface2

                            Rectangle {
                                width: 18
                                height: 18
                                radius: 9
                                color: Theme.text
                                anchors.verticalCenter: parent.verticalCenter
                                x: Notifications.dndEnabled ? parent.width - width - 2 : 2
                                Behavior on x { NumberAnimation { duration: 120 } }
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Notifications.toggleDnd()
                            }
                        }
                    }

                    // --- Mpris ---
                    RowLayout {
                        visible: Mpris.activePlayer !== null
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            text: "♫"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: 14
                        }

                        Text {
                            text: (Mpris.activePlayer?.trackTitle ?? "") + " — " + (Mpris.activePlayer?.trackArtist ?? "")
                            color: Theme.subtext0
                            font.family: Config.fontFamily
                            font.pixelSize: 12
                            Layout.fillWidth: true
                            elide: Text.ElideRight

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Mpris.playPause()
                            }
                        }
                    }

                    // --- Per-app volume sliders ---
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6
                        visible: playbackNodes.length > 0

                        readonly property var playbackNodes: {
                            const list = Pipewire.nodes.values;
                            return list.filter(n => n.isStream && n.audio &&
                                (n.type & PwNodeType.Audio) && (n.type & PwNodeType.Sink) && (n.type & PwNodeType.Stream));
                        }

                        Text {
                            text: "\u{F057E} Applications"
                            color: Theme.subtext0
                            font.family: Config.fontFamily
                            font.pixelSize: 12
                        }

                        PwObjectTracker {
                            objects: parent.playbackNodes
                        }

                        Repeater {
                            model: parent.playbackNodes
                            delegate: RowLayout {
                                required property var modelData
                                Layout.fillWidth: true
                                spacing: 8

                                Text {
                                    text: modelData.description || modelData.name
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: 11
                                    Layout.preferredWidth: 100
                                    elide: Text.ElideRight
                                }

                                Rectangle {
                                    Layout.fillWidth: true
                                    height: 5
                                    radius: 2.5
                                    color: Theme.surface2

                                    Rectangle {
                                        width: parent.width * Math.min(1, modelData.audio.volume)
                                        height: parent.height
                                        radius: 2.5
                                        color: Theme.blue
                                    }

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: mouse => {
                                            modelData.audio.volume = mouse.x / width;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // --- Notification list ---
                    Flickable {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        contentHeight: notifList.implicitHeight
                        clip: true

                        ColumnLayout {
                            id: notifList
                            width: parent.width
                            spacing: 8

                            Text {
                                visible: Notifications.trackedNotifications.values.length === 0
                                text: "No Notifications"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: 13
                                Layout.alignment: Qt.AlignHCenter
                                Layout.topMargin: 30
                            }

                            Repeater {
                                model: Notifications.trackedNotifications.values
                                delegate: Rectangle {
                                    required property var modelData
                                    Layout.fillWidth: true
                                    implicitHeight: itemContent.implicitHeight + 16
                                    radius: 8
                                    color: Theme.surface1

                                    ColumnLayout {
                                        id: itemContent
                                        anchors.fill: parent
                                        anchors.margins: 8
                                        spacing: 4

                                        RowLayout {
                                            Text {
                                                text: modelData.summary
                                                color: Theme.text
                                                font.family: Config.fontFamily
                                                font.bold: true
                                                font.pixelSize: 12
                                                Layout.fillWidth: true
                                                elide: Text.ElideRight
                                            }
                                            Text {
                                                text: "\u{F0156}"
                                                color: Theme.overlay0
                                                font.family: Config.fontFamily
                                                font.pixelSize: 12

                                                MouseArea {
                                                    anchors.fill: parent
                                                    onClicked: modelData.dismiss()
                                                }
                                            }
                                        }

                                        Text {
                                            visible: modelData.body !== ""
                                            text: modelData.body
                                            color: Theme.subtext0
                                            font.family: Config.fontFamily
                                            font.pixelSize: 11
                                            wrapMode: Text.WordWrap
                                            Layout.fillWidth: true
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
