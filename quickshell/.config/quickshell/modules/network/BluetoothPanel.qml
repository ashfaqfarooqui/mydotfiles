import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Bluetooth as QB
import qs.theme
import qs.config
import qs.services

// Native Bluetooth management panel, opened by left-clicking
// BluetoothWidget.qml (previously launched blueman-manager). API surface
// confirmed against https://quickshell.org/docs/v0.3.0/types/Quickshell.Bluetooth/
// — BluetoothAdapter.enabled/discovering are read-write (cross-checked
// against real usage in snowarch/iNiR and corecathx/whisker, since the
// doc fetch tool's own property-readonly/readwrite categorization proved
// unreliable), and BluetoothDevice.pair()/connect()/disconnect()/forget()
// are all first-party methods — no bluetoothctl involved.
Scope {
    id: root

    // Same cross-monitor gating as NetworkPanel.qml/TooltipBus.qml.
    required property string screenName
    readonly property bool isActive: Bluetooth.panelVisible && Bluetooth.panelScreenName === screenName

    readonly property var adapter: QB.Bluetooth.defaultAdapter ?? null
    readonly property var devices: {
        const list = adapter?.devices.values ?? [];
        return [...list].sort((a, b) => {
            if (a.connected !== b.connected) return a.connected ? -1 : 1;
            if (a.paired !== b.paired) return a.paired ? -1 : 1;
            return a.deviceName.localeCompare(b.deviceName);
        });
    }

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: Config.px(340)
            implicitHeight: Math.min(500, content.implicitHeight + 28)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                // Dismisses the panel on a click anywhere outside it — same
                // mechanism as NetworkPanel.qml/ControlCenter.qml.
                active: root.isActive
                windows: [win]
                onCleared: Bluetooth.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                // PanelWindow's layershell surface isn't a QtQuick Item, so
                // Keys can't attach there directly — an Item with
                // focus: true is required to receive routed key events (same
                // pattern as NetworkPanel.qml/ThemePicker.qml's keyCatcher).
                focus: true

                Keys.onEscapePressed: Bluetooth.hidePanel()

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "Bluetooth"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(15)
                            Layout.fillWidth: true
                        }

                        // Scan toggle — drives BluetoothAdapter.discovering,
                        // which populates `devices` with nearby discoverable
                        // devices in addition to already-paired ones.
                        Text {
                            visible: root.adapter?.enabled ?? false
                            text: root.adapter?.discovering ? "Stop" : "Scan"
                            color: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(11)

                            MouseArea {
                                anchors.fill: parent
                                onClicked: root.adapter.discovering = !root.adapter.discovering
                            }
                        }

                        Rectangle {
                            width: 40
                            height: 22
                            radius: 11
                            color: (root.adapter?.enabled ?? false) ? Theme.blue : Theme.surface2

                            Rectangle {
                                width: 18
                                height: 18
                                radius: 9
                                color: Theme.text
                                anchors.verticalCenter: parent.verticalCenter
                                x: (root.adapter?.enabled ?? false) ? parent.width - width - 2 : 2
                                Behavior on x { NumberAnimation { duration: 120 } }
                            }

                            MouseArea {
                                anchors.fill: parent
                                enabled: root.adapter !== null
                                onClicked: root.adapter.enabled = !root.adapter.enabled
                            }
                        }
                    }

                    Flickable {
                        Layout.fillWidth: true
                        // Layout.fillHeight doesn't feed back into the
                        // parent ColumnLayout's implicitHeight (which
                        // PanelWindow uses to size itself), so an
                        // explicit preferredHeight derived from the
                        // actual list content is required — otherwise
                        // the window collapses to just the header.
                        Layout.preferredHeight: Math.min(320, list.implicitHeight)
                        contentHeight: list.implicitHeight
                        clip: true
                        visible: root.adapter?.enabled ?? false

                        ColumnLayout {
                            id: list
                            width: parent.width
                            spacing: 4

                            Text {
                                visible: root.devices.length === 0
                                text: root.adapter?.discovering ? "Scanning…" : "No devices"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(12)
                                Layout.topMargin: 20
                                Layout.alignment: Qt.AlignHCenter
                            }

                            Repeater {
                                model: root.devices
                                delegate: Rectangle {
                                    required property var modelData
                                    Layout.fillWidth: true
                                    implicitHeight: rowContent.implicitHeight + 16
                                    radius: 8
                                    color: modelData.connected ? Theme.surface1 : "transparent"

                                    RowLayout {
                                        id: rowContent
                                        anchors.fill: parent
                                        anchors.margins: 8
                                        spacing: 8

                                        Text {
                                            text: "\u{F00E1}"
                                            color: Theme.text
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(14)
                                        }

                                        Text {
                                            text: modelData.name !== "" ? modelData.name : modelData.deviceName
                                            color: Theme.text
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(12)
                                            Layout.fillWidth: true
                                            elide: Text.ElideRight
                                        }

                                        Text {
                                            visible: modelData.batteryAvailable
                                            text: Math.round(modelData.battery * 100) + "%"
                                            color: Theme.subtext0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(11)
                                        }

                                        Text {
                                            visible: modelData.pairing
                                            text: "Pairing…"
                                            color: Theme.subtext0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(11)
                                        }

                                        Rectangle {
                                            implicitWidth: actionLabel.implicitWidth + 16
                                            implicitHeight: Config.px(22)
                                            radius: 6
                                            color: modelData.connected ? Theme.surface2 : Theme.blue

                                            Text {
                                                id: actionLabel
                                                anchors.centerIn: parent
                                                text: modelData.connected ? "Disconnect" : (modelData.paired ? "Connect" : "Pair")
                                                color: modelData.connected ? Theme.text : Theme.crust
                                                font.family: Config.fontFamily
                                                font.pixelSize: Config.px(10)
                                                font.bold: true
                                            }

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: {
                                                    if (modelData.connected) modelData.disconnect();
                                                    else if (modelData.paired) modelData.connect();
                                                    else modelData.pair();
                                                }
                                            }
                                        }

                                        Text {
                                            visible: modelData.paired
                                            text: "\u{F0156}"
                                            color: Theme.overlay0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(12)

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: modelData.forget()
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
