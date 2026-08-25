import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services
import qs.modules.bar

// Tailscale status + peer browser, opened by left-clicking
// TailscaleWidget.qml. Ported down from basecamp/omarchy's
// shell/plugins/panels/tailscale: status, up/down toggle (with browser-auth
// handoff), online-peer list with IP/name/DNS copy, an exit-node/Mullvad
// relay picker, and Taildrop send — no account switching (multiple
// tailnets/profiles), matching NetworkPanel.qml's own "bar status panel, not
// a full client" scoping.
Scope {
    id: root

    required property string screenName
    readonly property bool isActive: Tailscale.panelVisible && Tailscale.panelScreenName === screenName
    property bool exitNodeOpen: false

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: Config.px(320)
            implicitHeight: Math.min(560, content.implicitHeight + 28)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: Tailscale.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Tailscale.hidePanel()

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        TailscaleIcon {
                            iconSize: Config.px(18)
                            tint: Tailscale.needsLogin ? Theme.yellow : (Tailscale.active ? Theme.blue : Theme.overlay0)
                        }

                        ColumnLayout {
                            spacing: 0
                            Layout.fillWidth: true

                            Text {
                                text: "Tailscale"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(15)
                            }
                            Text {
                                text: Tailscale.actionStatus !== "" ? Tailscale.actionStatus : Tailscale.statusText
                                color: Tailscale.lastError !== "" ? Theme.red : Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                            }
                        }

                        Rectangle {
                            width: 40
                            height: 22
                            radius: 11
                            color: Tailscale.active ? Theme.blue : Theme.surface2

                            Rectangle {
                                width: 18
                                height: 18
                                radius: 9
                                color: Theme.text
                                anchors.verticalCenter: parent.verticalCenter
                                x: Tailscale.active ? parent.width - width - 2 : 2
                                Behavior on x { NumberAnimation { duration: 120 } }
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Tailscale.toggle()
                            }
                        }
                    }

                    Text {
                        visible: Tailscale.selfIp !== ""
                        text: Tailscale.selfName + "  ·  " + Tailscale.selfIp
                        color: Theme.subtext0
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(11)
                        Layout.fillWidth: true
                        elide: Text.ElideRight
                    }

                    // The tailscaled daemon on this machine runs as root with
                    // no operator set, so `up`/`down`/exit-node/Taildrop all
                    // fail with "Access denied" until authorized once — see
                    // Tailscale.qml's needsOperatorAuth.
                    RowLayout {
                        visible: Tailscale.needsOperatorAuth
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            text: "Needs operator access to control Tailscale"
                            color: Theme.yellow
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(11)
                            wrapMode: Text.WordWrap
                            Layout.fillWidth: true
                        }

                        Rectangle {
                            implicitWidth: authorizeLabel.implicitWidth + 16
                            implicitHeight: Config.px(22)
                            radius: 6
                            color: Theme.blue

                            Text {
                                id: authorizeLabel
                                anchors.centerIn: parent
                                text: "Authorize"
                                color: Theme.crust
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(10)
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Tailscale.authorizeOperator()
                            }
                        }
                    }

                    Rectangle { Layout.fillWidth: true; height: 1; color: Theme.surface2 }

                    // EXIT NODE: current selection + expand/collapse list of
                    // tailnet peers (that offer themselves as one) and
                    // deduped Mullvad relay cities.
                    RowLayout {
                        visible: Tailscale.active
                        Layout.fillWidth: true
                        spacing: 6

                        Text {
                            text: "EXIT NODE"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(10)
                        }

                        Text {
                            text: Tailscale.activeExitNodeLabel
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(11)
                            elide: Text.ElideRight
                            Layout.fillWidth: true
                        }

                        Text {
                            text: root.exitNodeOpen ? "\u{F0143}" : "\u{F0140}" // md-chevron_up / md-chevron_down
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(13)

                            MouseArea {
                                anchors.fill: parent
                                onClicked: root.exitNodeOpen = !root.exitNodeOpen
                            }
                        }
                    }

                    ColumnLayout {
                        visible: Tailscale.active && root.exitNodeOpen
                        Layout.fillWidth: true
                        spacing: 2

                        component ExitNodeRow: Rectangle {
                            id: exitRow
                            property string label
                            property bool isActive
                            signal activated()
                            Layout.fillWidth: true
                            implicitHeight: 26
                            radius: 6
                            color: exitRow.isActive ? Theme.surface1 : "transparent"

                            RowLayout {
                                anchors.fill: parent
                                anchors.margins: 4
                                spacing: 6

                                Text {
                                    text: exitRow.label
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(11)
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }
                                Text {
                                    visible: exitRow.isActive
                                    text: "\u{F012C}"
                                    color: Theme.blue
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(12)
                                }
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: exitRow.activated()
                            }
                        }

                        ExitNodeRow {
                            label: "None"
                            isActive: Tailscale.activeExitNode === null
                            onActivated: { Tailscale.setExitNode(null); root.exitNodeOpen = false; }
                        }

                        Repeater {
                            model: Tailscale.exitNodeOptions
                            delegate: ExitNodeRow {
                                required property var modelData
                                label: modelData.hostName
                                isActive: Tailscale.activeExitNode?.id === modelData.id
                                onActivated: { Tailscale.setExitNode(modelData); root.exitNodeOpen = false; }
                            }
                        }
                    }

                    Rectangle { visible: Tailscale.active; Layout.fillWidth: true; height: 1; color: Theme.surface2 }

                    Flickable {
                        Layout.fillWidth: true
                        Layout.preferredHeight: Math.min(260, list.implicitHeight)
                        contentHeight: list.implicitHeight
                        clip: true
                        visible: Tailscale.active

                        ColumnLayout {
                            id: list
                            width: parent.width
                            spacing: 4

                            Text {
                                visible: Tailscale.peers.length === 0
                                text: "No peers online"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(12)
                                Layout.topMargin: 20
                                Layout.alignment: Qt.AlignHCenter
                            }

                            Repeater {
                                model: Tailscale.peers
                                delegate: Rectangle {
                                    required property var modelData
                                    Layout.fillWidth: true
                                    implicitHeight: rowContent.implicitHeight + 16
                                    radius: 8
                                    color: rowHover.hovered ? Theme.surface1 : "transparent"

                                    HoverHandler { id: rowHover }

                                    RowLayout {
                                        id: rowContent
                                        anchors.fill: parent
                                        anchors.margins: 8
                                        spacing: 8

                                        Text {
                                            text: Tailscale.osIcon(modelData.os)
                                            color: Theme.text
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(14)
                                        }

                                        ColumnLayout {
                                            spacing: 0
                                            Layout.fillWidth: true

                                            Text {
                                                text: modelData.hostName
                                                color: Theme.text
                                                font.family: Config.fontFamily
                                                font.pixelSize: Config.px(12)
                                                elide: Text.ElideRight
                                                Layout.fillWidth: true
                                            }
                                            Text {
                                                text: modelData.ip
                                                color: Theme.overlay0
                                                font.family: Config.fontFamily
                                                font.pixelSize: Config.px(10)
                                            }
                                        }

                                        // Send via Taildrop (only when the tailnet allows file
                                        // sharing and this peer is a valid target).
                                        Text {
                                            visible: Tailscale.canSendFiles(modelData)
                                            text: "\u{F048A}" // nf-md-send
                                            color: Theme.blue
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(13)

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: Tailscale.sendFile(modelData)
                                            }
                                        }

                                        // Copy DNS name.
                                        Text {
                                            visible: modelData.dnsName !== ""
                                            text: "\u{F01D6}" // nf-md-dns
                                            color: Theme.overlay0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(13)

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: Tailscale.copyPeerDnsName(modelData)
                                            }
                                        }

                                        // Copy IP.
                                        Text {
                                            text: "\u{F018F}" // nf-md-content_copy
                                            color: Theme.overlay0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(13)

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: Tailscale.copyPeerIp(modelData)
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
}
