import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Services.UPower
import qs.theme
import qs.config
import qs.services

// Native battery/power-profile panel, opened by left-clicking
// BatteryWidget.qml. Same top-right anchor/dismiss chrome as
// NetworkPanel.qml/BluetoothPanel.qml. PowerProfiles API confirmed against
// https://quickshell.org/docs/v0.3.0/types/Quickshell.Services.UPower/PowerProfiles/
// — profile is a directly settable property, PowerProfile enum members are
// PowerSaver/Balanced/Performance.
Scope {
    id: root

    // Same cross-monitor gating as NetworkPanel.qml/TooltipBus.qml.
    required property string screenName
    readonly property bool isActive: Battery.panelVisible && Battery.panelScreenName === screenName

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            // 300 was too narrow for three power-profile chips with full
            // labels ("Power-saver"/"Performance") side by side with their
            // icons — the widest chip's content overflowed its own
            // Rectangle and visually overlapped its neighbor.
            implicitWidth: Config.px(320)
            implicitHeight: content.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: Battery.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Battery.hidePanel()

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
                    spacing: 10

                    // Pill button used by the power-profile row below. Adds
                    // a `selected` visual state on top of NetworkPanel.qml's
                    // original Chip.
                    component Chip: Rectangle {
                        id: chip
                        property string icon
                        property string label
                        property bool selected: false
                        signal activated()
                        Layout.fillWidth: true
                        implicitHeight: Config.px(26)
                        radius: 6
                        color: selected ? Theme.blue : Theme.surface1

                        // Icon and label as separate Text elements in a
                        // centered Row, not one concatenated string — a
                        // Nerd Font glyph's metrics don't line up with
                        // regular text sharing the same anchors.centerIn
                        // baseline, which read as visually misaligned.
                        RowLayout {
                            anchors.centerIn: parent
                            spacing: 4

                            Text {
                                text: chip.icon
                                color: chip.selected ? Theme.crust : Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(11)
                            }
                            Text {
                                text: chip.label
                                color: chip.selected ? Theme.crust : Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                                font.bold: chip.selected
                                elide: Text.ElideRight
                            }
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: chip.activated()
                        }
                    }

                    // Hero row: icon + title + status caption, big percent
                    // right-aligned.
                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 10

                        Text {
                            text: Battery.iconFor(Math.round(Battery.percent), Battery.charging)
                            color: Battery.percent <= 15 && !Battery.charging ? Theme.red : Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(22)
                        }

                        ColumnLayout {
                            spacing: 0
                            Text {
                                text: "Battery"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(15)
                            }
                            Text {
                                text: Battery.displayState.toUpperCase()
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                            }
                        }

                        Item { Layout.fillWidth: true }

                        Text {
                            text: Math.round(Battery.percent) + "%"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(22)
                        }
                    }

                    // Charge progress bar.
                    Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 8
                        radius: 4
                        color: Theme.surface2

                        Rectangle {
                            anchors.left: parent.left
                            anchors.top: parent.top
                            anchors.bottom: parent.bottom
                            width: parent.width * (Battery.percent / 100)
                            radius: 4
                            color: Battery.percent <= 15 && !Battery.charging ? Theme.red : Theme.blue
                        }
                    }

                    // Two-column stat grid.
                    GridLayout {
                        Layout.fillWidth: true
                        Layout.topMargin: 2
                        columns: 2
                        rowSpacing: 4
                        columnSpacing: 12

                        component StatText: RowLayout {
                            Layout.fillWidth: true
                            property string label
                            property string value
                            Text { text: parent.label; color: Theme.subtext0; font.family: Config.fontFamily; font.pixelSize: Config.px(11); Layout.fillWidth: true }
                            Text { text: parent.value; color: Theme.text; font.family: Config.fontFamily; font.bold: true; font.pixelSize: Config.px(11) }
                        }

                        StatText { visible: Battery.capacityWh > 0; label: "Battery size"; value: Battery.capacityWh.toFixed(0) + "Wh" }
                        StatText {
                            visible: Battery.chargeThresholdSet
                            label: "Charge limit"
                            value: Battery.chargeThresholdStart > 0 && Battery.chargeThresholdStart !== Battery.chargeThresholdEnd
                                ? Battery.chargeThresholdStart + "-" + Battery.chargeThresholdEnd + "%"
                                : Battery.chargeThresholdEnd + "%"
                        }
                        StatText { visible: Battery.cycleCount > 0; label: "Charge cycles"; value: String(Battery.cycleCount) }
                        StatText { label: "Battery state"; value: Battery.displayState }
                        StatText {
                            visible: Battery.charging && Battery.timeToFullFormatted !== ""
                            label: "Time to full"; value: Battery.timeToFullFormatted
                        }
                        StatText {
                            visible: !Battery.charging && Battery.timeToEmptyFormatted !== ""
                            label: "Time remaining"; value: Battery.timeToEmptyFormatted
                        }
                    }

                    Rectangle { Layout.fillWidth: true; Layout.topMargin: 4; height: 1; color: Theme.surface2 }

                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6

                        Text {
                            text: "POWER PROFILE"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(10)
                            font.bold: true
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 6

                            Chip {
                                icon: "\u{F032A}" // nf-md-leaf
                                label: "Power-saver"
                                selected: PowerProfiles.profile === PowerProfile.PowerSaver
                                onActivated: Battery.setPowerProfile(PowerProfile.PowerSaver)
                            }
                            Chip {
                                icon: "\u{F05D1}" // nf-md-scale_balance
                                label: "Balanced"
                                selected: PowerProfiles.profile === PowerProfile.Balanced
                                onActivated: Battery.setPowerProfile(PowerProfile.Balanced)
                            }
                            Chip {
                                icon: "\u{F04C5}" // nf-md-speedometer
                                label: "Performance"
                                selected: PowerProfiles.profile === PowerProfile.Performance
                                visible: PowerProfiles.hasPerformanceProfile
                                onActivated: Battery.setPowerProfile(PowerProfile.Performance)
                            }
                        }
                    }
                }
            }
        }
    }
}
