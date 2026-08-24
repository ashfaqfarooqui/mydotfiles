import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Combined CPU/Memory/Disk/Temperature/GPU/process view, opened by clicking
// any of CpuWidget/MemoryWidget/DiskWidget/TemperatureWidget — modeled on
// Omarchy's "Vitals" panel screenshot. Same top-right anchor/dismiss chrome
// as BatteryPanel.qml/etc, wider (360px) to fit the per-core bar row and
// process list.
Scope {
    id: root

    required property string screenName
    readonly property bool isActive: SystemStats.panelVisible && SystemStats.panelScreenName === screenName

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: Config.px(360)
            implicitHeight: content.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: SystemStats.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: SystemStats.hidePanel()

                MouseArea {
                    anchors.fill: parent
                    z: -1
                }

                component SectionLabel: Text {
                    color: Theme.overlay0
                    font.family: Config.fontFamily
                    font.pixelSize: Config.px(10)
                    font.bold: true
                }

                component StatRow: RowLayout {
                    Layout.fillWidth: true
                    spacing: 12

                    property string leftLabel
                    property string leftValue
                    property string rightLabel
                    property string rightValue

                    RowLayout {
                        spacing: 4
                        Text { text: parent.parent.leftLabel; color: Theme.subtext0; font.family: Config.fontFamily; font.pixelSize: Config.px(11) }
                        Text { text: parent.parent.leftValue; color: Theme.text; font.family: Config.fontFamily; font.bold: true; font.pixelSize: Config.px(11) }
                    }
                    Item { Layout.fillWidth: true }
                    RowLayout {
                        visible: parent.rightLabel !== ""
                        spacing: 4
                        Text { text: parent.parent.rightLabel; color: Theme.subtext0; font.family: Config.fontFamily; font.pixelSize: Config.px(11) }
                        Text { text: parent.parent.rightValue; color: Theme.text; font.family: Config.fontFamily; font.bold: true; font.pixelSize: Config.px(11) }
                    }
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 12

                    // Hero row
                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 10

                        Text {
                            text: "\u{F061A}" // nf-md-chip (processor)
                            color: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(24)
                        }

                        ColumnLayout {
                            spacing: 0
                            Text {
                                text: "Vitals"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(15)
                            }
                            Text {
                                readonly property bool warning: SystemStats.cpuPercent >= 85 || SystemStats.memPercent >= 90 || SystemStats.tempC >= 85
                                text: warning ? "ELEVATED" : "NOMINAL"
                                color: warning ? Theme.yellow : Theme.green
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                            }
                        }

                        Item { Layout.fillWidth: true }

                        Rectangle {
                            radius: 6
                            color: Theme.surface1
                            implicitWidth: badgeText.implicitWidth + 16
                            implicitHeight: Config.px(22)
                            Text {
                                id: badgeText
                                anchors.centerIn: parent
                                text: SystemStats.cpuPercent + "% · " + SystemStats.memUsedGB.toFixed(1) + " / " + SystemStats.memTotalGB.toFixed(1) + " GB"
                                color: Theme.subtext1
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                            }
                        }
                    }

                    // PROCESSOR
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6

                        SectionLabel { text: "PROCESSOR" }
                        StatRow {
                            leftLabel: "Load"; leftValue: SystemStats.cpuPercent + "%"
                            rightLabel: "Memory"; rightValue: SystemStats.memUsedGB.toFixed(1) + " / " + SystemStats.memTotalGB.toFixed(1) + " GB"
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            Layout.topMargin: 2
                            spacing: 2
                            visible: SystemStats.perCorePercent.length > 0

                            Repeater {
                                model: SystemStats.perCorePercent
                                delegate: Rectangle {
                                    required property real modelData
                                    Layout.fillWidth: true
                                    Layout.preferredHeight: 18
                                    radius: 2
                                    color: Theme.surface2

                                    Rectangle {
                                        anchors.bottom: parent.bottom
                                        anchors.left: parent.left
                                        anchors.right: parent.right
                                        height: Math.max(2, parent.height * (modelData / 100))
                                        radius: 2
                                        color: modelData >= 90 ? Theme.red : (modelData >= 70 ? Theme.yellow : Theme.blue)
                                    }
                                }
                            }
                        }

                        Text {
                            visible: SystemStats.perCorePercent.length > 0
                            Layout.alignment: Qt.AlignRight
                            text: SystemStats.perCorePercent.length + " threads"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(9)
                        }
                    }

                    // HEAVIEST PROCESSES
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4
                        visible: Processes.topProcesses.length > 0

                        SectionLabel { text: "HEAVIEST PROCESSES" }

                        Repeater {
                            model: Processes.topProcesses
                            delegate: RowLayout {
                                required property var modelData
                                Layout.fillWidth: true

                                Text {
                                    text: modelData.name
                                    color: Theme.subtext1
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(11)
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }
                                Text {
                                    text: modelData.cpu.toFixed(1) + "%"
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(11)
                                }
                            }
                        }

                        Text {
                            Layout.alignment: Qt.AlignRight
                            text: "100% is one thread fully used"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(9)
                        }
                    }

                    // GRAPHICS
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6
                        visible: Gpu.available

                        SectionLabel { text: "GRAPHICS" }
                        StatRow {
                            leftLabel: "Busy"; leftValue: Gpu.busyPercent + "%"
                            rightLabel: "Video memory"; rightValue: Gpu.vramUsedGB.toFixed(1) + " / " + Gpu.vramTotalGB.toFixed(1) + " GB"
                        }
                    }

                    // TEMPERATURES
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6

                        SectionLabel { text: "TEMPERATURES" }
                        StatRow {
                            leftLabel: "Processor"; leftValue: SystemStats.tempC + "°C"
                            rightLabel: SystemStats.graphicsTempAvailable ? "Graphics" : ""
                            rightValue: SystemStats.graphicsTempAvailable ? SystemStats.graphicsTempC + "°C" : ""
                        }
                        StatRow {
                            visible: SystemStats.diskTempAvailable || SystemStats.fanAvailable
                            leftLabel: SystemStats.diskTempAvailable ? "Disk" : ""
                            leftValue: SystemStats.diskTempAvailable ? SystemStats.diskTempC + "°C" : ""
                            rightLabel: SystemStats.fanAvailable ? "Fan" : ""
                            rightValue: SystemStats.fanAvailable ? SystemStats.fanRpm + " rpm" : ""
                        }
                    }

                    // STORAGE — NVMe SMART health via udisks2 (see
                    // services/DiskHealth.qml), not just usage/temperature.
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6
                        visible: DiskHealth.available

                        SectionLabel { text: "STORAGE" }
                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 4
                            Text {
                                text: "Health"
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(11)
                            }
                            Text {
                                text: DiskHealth.healthy ? "OK" : "Warning: " + DiskHealth.warnings.join(", ")
                                color: DiskHealth.healthy ? Theme.green : Theme.red
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(11)
                                Layout.fillWidth: true
                                Layout.alignment: Qt.AlignRight
                                horizontalAlignment: Text.AlignRight
                                elide: Text.ElideLeft
                            }
                        }
                        StatRow {
                            leftLabel: "Power-on time"
                            leftValue: Math.floor(DiskHealth.powerOnHours / 24) + "d " + (DiskHealth.powerOnHours % 24) + "h"
                        }
                    }
                }
            }
        }
    }
}
