import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Native pipewire mixer panel, opened by left-clicking VolumeWidget.qml.
// Same top-right anchor/dismiss chrome as NetworkPanel.qml/BatteryPanel.qml.
// Sliders are hand-rolled (see BrightnessPanel.qml's header comment —
// QtQuick.Controls isn't installed on this system) and device chips reuse
// BatteryPanel.qml's Chip component pattern.
Scope {
    id: root

    // Same cross-monitor gating as NetworkPanel.qml/TooltipBus.qml.
    required property string screenName
    readonly property bool isActive: Audio.panelVisible && Audio.panelScreenName === screenName

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: 340
            implicitHeight: content.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: Audio.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Audio.hidePanel()

                MouseArea {
                    // Eats clicks that land inside this window but outside
                    // any child control (same as NetworkPanel.qml).
                    anchors.fill: parent
                    z: -1
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 12

                    component Chip: Rectangle {
                        property string label
                        property bool selected: false
                        signal activated()
                        Layout.fillWidth: true
                        implicitHeight: 22
                        radius: 6
                        color: selected ? Theme.blue : Theme.surface1

                        Text {
                            anchors.centerIn: parent
                            anchors.margins: 4
                            width: parent.width - 8
                            elide: Text.ElideRight
                            horizontalAlignment: Text.AlignHCenter
                            text: parent.label
                            color: parent.selected ? Theme.crust : Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: 10
                            font.bold: parent.selected
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: parent.activated()
                        }
                    }

                    component VolumeSlider: Item {
                        id: slider
                        Layout.fillWidth: true
                        implicitHeight: 20
                        property real ratio: 0
                        property bool danger: false
                        signal moved(real ratio)

                        function setFromX(x) {
                            slider.moved(Math.max(0, Math.min(1, x / slider.width)));
                        }

                        Rectangle {
                            id: track
                            anchors.verticalCenter: parent.verticalCenter
                            width: parent.width
                            height: 6
                            radius: 3
                            color: Theme.surface2

                            Rectangle {
                                width: track.width * slider.ratio
                                height: track.height
                                radius: 3
                                color: slider.danger ? Theme.red : Theme.blue
                            }
                        }

                        Rectangle {
                            width: 14
                            height: 14
                            radius: 7
                            color: Theme.text
                            anchors.verticalCenter: parent.verticalCenter
                            x: Math.max(0, Math.min(slider.width - width, track.width * slider.ratio - width / 2))
                        }

                        MouseArea {
                            anchors.fill: parent
                            onPressed: mouse => slider.setFromX(mouse.x)
                            onPositionChanged: mouse => { if (pressed) slider.setFromX(mouse.x); }
                        }
                    }

                    // ---- Output ----
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 8

                            Text {
                                text: Audio.muted ? "" : ""
                                color: Audio.muted ? Theme.overlay0 : Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: 15
                                MouseArea { anchors.fill: parent; onClicked: Audio.toggleMute() }
                            }

                            Text {
                                text: "Output"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: 13
                                Layout.fillWidth: true
                            }

                            Text {
                                text: Audio.volumePercent + "%"
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: 11
                            }
                        }

                        VolumeSlider {
                            ratio: Math.min(1, Audio.volume)
                            danger: Audio.volumePercent > 100
                            onMoved: r => Audio.setVolume(r)
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 6
                            Repeater {
                                model: Audio.outputDevices
                                delegate: Chip {
                                    required property var modelData
                                    label: Audio.streamLabel(modelData)
                                    selected: modelData === Audio.sink
                                    onActivated: Audio.setOutputDevice(modelData)
                                }
                            }
                        }
                    }

                    Rectangle { Layout.fillWidth: true; implicitHeight: 1; color: Theme.surface2 }

                    // ---- Input ----
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 8

                            Text {
                                text: ""
                                color: Audio.sourceMuted ? Theme.overlay0 : Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: 15
                                MouseArea { anchors.fill: parent; onClicked: Audio.toggleSourceMute() }
                            }

                            Text {
                                text: "Input"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: 13
                                Layout.fillWidth: true
                            }

                            Text {
                                text: Audio.sourceVolumePercent + "%"
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: 11
                            }
                        }

                        VolumeSlider {
                            ratio: Math.min(1, Audio.sourceVolume)
                            onMoved: r => Audio.setSourceVolume(r)
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 6
                            Repeater {
                                model: Audio.inputDevices
                                delegate: Chip {
                                    required property var modelData
                                    label: Audio.streamLabel(modelData)
                                    selected: modelData === Audio.source
                                    onActivated: Audio.setInputDevice(modelData)
                                }
                            }
                        }
                    }

                    // ---- Per-app mixer ----
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4
                        visible: Audio.streams.length > 0

                        Rectangle { Layout.fillWidth: true; implicitHeight: 1; color: Theme.surface2 }

                        Text {
                            text: "Applications"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 10
                            font.bold: true
                        }

                        Repeater {
                            model: Audio.streams
                            delegate: ColumnLayout {
                                id: streamRow
                                required property var modelData
                                Layout.fillWidth: true
                                spacing: 2

                                RowLayout {
                                    Layout.fillWidth: true
                                    spacing: 8

                                    Text {
                                        text: streamRow.modelData.audio.muted ? "" : ""
                                        color: streamRow.modelData.audio.muted ? Theme.overlay0 : Theme.text
                                        font.family: Config.fontFamily
                                        font.pixelSize: 12
                                        MouseArea {
                                            anchors.fill: parent
                                            onClicked: streamRow.modelData.audio.muted = !streamRow.modelData.audio.muted
                                        }
                                    }

                                    Text {
                                        text: Audio.streamLabel(streamRow.modelData)
                                        color: Theme.text
                                        font.family: Config.fontFamily
                                        font.pixelSize: 11
                                        elide: Text.ElideRight
                                        Layout.fillWidth: true
                                    }

                                    Text {
                                        text: Math.round((streamRow.modelData.audio.volume ?? 0) * 100) + "%"
                                        color: Theme.subtext0
                                        font.family: Config.fontFamily
                                        font.pixelSize: 10
                                    }
                                }

                                VolumeSlider {
                                    ratio: Math.min(1, streamRow.modelData.audio.volume ?? 0)
                                    onMoved: r => { if (streamRow.modelData.audio) streamRow.modelData.audio.volume = r; }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
