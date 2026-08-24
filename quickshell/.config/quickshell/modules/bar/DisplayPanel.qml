import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Native Display panel, opened by left-clicking BacklightWidget.qml —
// replaces the old bare BrightnessPanel.qml with an Omarchy-style panel
// (basecamp/omarchy's quattro-branch shell/plugins/panels/monitor/Panel.qml):
// hero header, brightness slider, text-size slider, scale presets, and a
// list of connected displays. Same top-right anchor/dismiss chrome as every
// other panel in this directory. QtQuick.Controls isn't installed on this
// system, so the slider is hand-rolled (see the original BrightnessPanel.qml
// this was built from for the pattern).
Scope {
    id: root

    // Same cross-monitor gating as NetworkPanel.qml/TooltipBus.qml.
    required property string screenName
    readonly property bool isActive: Brightness.panelVisible && Brightness.panelScreenName === screenName

    // The monitor THIS panel is about — previously every section here read
    // MonitorScale.focusedMonitor (whichever monitor Hyprland currently
    // considers globally focused), which silently showed/controlled the
    // wrong monitor's scale whenever you opened the Display panel from a
    // monitor that wasn't the Hyprland-focused one.
    readonly property var panelMonitor: Hypr.monitors.values.find(m => m.name === root.screenName) ?? null

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: 320
            implicitHeight: content.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: Brightness.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Brightness.hidePanel()

                MouseArea {
                    // Eats clicks that land inside this window but outside
                    // any child control (same as NetworkPanel.qml).
                    anchors.fill: parent
                    z: -1
                }

                // Uppercase dim label + right-aligned live value, matching
                // the screenshot's "BRIGHTNESS ... 100%" section headers.
                component SectionHeader: RowLayout {
                    property string label
                    property string value

                    Text {
                        text: parent.label
                        color: Theme.overlay0
                        font.family: Config.fontFamily
                        font.bold: true
                        font.pixelSize: 10
                        Layout.fillWidth: true
                    }
                    Text {
                        text: parent.value
                        color: Theme.subtext0
                        font.family: Config.fontFamily
                        font.pixelSize: 11
                    }
                }

                // Draggable-handle slider taking a normalized 0..1 ratio,
                // reused for both brightness and text size below (same
                // track/handle visual as the original BrightnessPanel.qml).
                component Slider: Item {
                    id: slider
                    property real ratio: 0
                    signal moved(real ratio)
                    Layout.fillWidth: true
                    implicitHeight: 20

                    function setFromX(x) {
                        moved(Math.max(0, Math.min(1, x / slider.width)));
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
                            color: Theme.blue
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
                        onPositionChanged: mouse => {
                            if (pressed) slider.setFromX(mouse.x);
                        }
                    }
                }

                // Scale preset pill, same selected/unselected shape as
                // BatteryPanel.qml's power-profile Chip.
                component ScalePill: Rectangle {
                    property string label
                    property bool selected: false
                    signal activated()
                    Layout.fillWidth: true
                    implicitHeight: 26
                    radius: 6
                    color: selected ? Theme.blue : Theme.surface1

                    Text {
                        anchors.centerIn: parent
                        text: parent.label
                        color: parent.selected ? Theme.crust : Theme.text
                        font.family: Config.fontFamily
                        font.pixelSize: 11
                        font.bold: parent.selected
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: parent.activated()
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
                            text: "\u{F0DDD}"
                            color: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: 22
                        }

                        ColumnLayout {
                            spacing: 0
                            Text {
                                text: "Display"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: 15
                            }
                            Text {
                                text: (root.panelMonitor?.description ?? root.panelMonitor?.name ?? "").toUpperCase()
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: 10
                                elide: Text.ElideRight
                                Layout.maximumWidth: 220
                            }
                        }
                    }

                    // Brightness (laptop-panel backlight only — see
                    // services/Brightness.qml).
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6
                        SectionHeader { Layout.fillWidth: true; label: "BRIGHTNESS"; value: Brightness.percent + "%" }
                        Slider {
                            ratio: Brightness.percent / 100
                            onMoved: r => Brightness.set(Math.round(r * 100))
                        }
                    }

                    // Text size — Omarchy's own stop list (a raw px value
                    // fed straight into the font system, not a scale
                    // multiplier — see Model.js's textSizeStops). A global
                    // Settings.fontSize, not per-monitor: DPI/zoom is a
                    // monitor property and lives in the Scale section below;
                    // this is just "how big should bar text read."
                    ColumnLayout {
                        id: textSizeSection
                        Layout.fillWidth: true
                        spacing: 6
                        readonly property var stops: [9, 10, 11, 12, 14, 16, 20]
                        readonly property int stopIndex: Math.max(0, stops.indexOf(Settings.fontSize))

                        SectionHeader { Layout.fillWidth: true; label: "TEXT SIZE"; value: Settings.fontSize + "px" }
                        Slider {
                            ratio: textSizeSection.stopIndex / (textSizeSection.stops.length - 1)
                            onMoved: r => {
                                const idx = Math.round(r * (textSizeSection.stops.length - 1));
                                Settings.fontSize = textSizeSection.stops[idx];
                            }
                        }
                    }

                    // Scale presets for THIS panel's monitor (root.panelMonitor,
                    // not whichever one Hyprland globally considers focused).
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 6
                        visible: root.panelMonitor !== null

                        SectionHeader {
                            Layout.fillWidth: true
                            label: "SCALE"
                            value: root.panelMonitor?.name ?? ""
                        }

                        GridLayout {
                            Layout.fillWidth: true
                            columns: 3
                            rowSpacing: 6
                            columnSpacing: 6

                            Repeater {
                                model: MonitorScale.scalePresetsFor(root.panelMonitor)
                                delegate: ScalePill {
                                    required property real modelData
                                    label: modelData.toFixed(2).replace(/\.?0+$/, "") + "x"
                                    selected: root.panelMonitor !== null
                                        && Math.abs(root.panelMonitor.scale - modelData) < 0.02
                                    onActivated: MonitorScale.setScale(root.panelMonitor, modelData)
                                }
                            }
                        }
                    }

                    // Connected displays — informational only. Distinguishes
                    // "this display" (the monitor this panel was opened
                    // from, root.screenName) from "active" (whichever
                    // monitor Hyprland currently considers globally focused)
                    // — they're often not the same monitor. "This display"
                    // is shown via a contrasting row background rather than
                    // a text label, so it reads at a glance instead of
                    // competing with the "· active" text for the same row.
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4
                        visible: Hypr.monitors.values.length > 1

                        SectionHeader { Layout.fillWidth: true; label: "DISPLAYS"; value: "" }

                        Repeater {
                            model: Hypr.monitors.values
                            delegate: Rectangle {
                                id: displayRow
                                required property var modelData
                                readonly property bool isThisPanel: modelData.name === root.screenName
                                Layout.fillWidth: true
                                implicitHeight: rowContent.implicitHeight + 8
                                radius: 6
                                color: isThisPanel ? Theme.surface2 : "transparent"

                                RowLayout {
                                    id: rowContent
                                    anchors.left: parent.left
                                    anchors.right: parent.right
                                    anchors.verticalCenter: parent.verticalCenter
                                    anchors.leftMargin: 6
                                    anchors.rightMargin: 6
                                    spacing: 8

                                    Text {
                                        text: "\u{F0DDD}"
                                        color: modelData.focused ? Theme.blue : Theme.overlay0
                                        font.family: Config.fontFamily
                                        font.pixelSize: 13
                                    }
                                    Text {
                                        text: modelData.name + (modelData.focused ? " · active" : "")
                                        color: modelData.focused ? Theme.text : Theme.subtext0
                                        font.family: Config.fontFamily
                                        font.pixelSize: 12
                                        font.bold: displayRow.isThisPanel
                                        Layout.fillWidth: true
                                    }
                                    Text {
                                        visible: modelData.focused
                                        text: "\u{F012C}"
                                        color: Theme.blue
                                        font.family: Config.fontFamily
                                        font.pixelSize: 12
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
