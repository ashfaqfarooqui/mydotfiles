import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// "Agents" (Claude Code usage) panel, opened by clicking AgentsWidget.qml.
// Local-data-only: Tokens-by-day and Tokens-by-model, both aggregated from
// ~/.claude/projects transcripts by scripts/agents-usage.py (see
// services/AgentsUsage.qml). No live Session/Weekly quota % bars — those
// need Anthropic's undocumented OAuth usage endpoint, scoped out.
// Same LazyLoader/PanelWindow/card skeleton as every other panel here.
Scope {
    id: root

    required property string screenName
    readonly property bool isActive: AgentsUsage.panelVisible && AgentsUsage.panelScreenName === screenName

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: Config.px(340)
            implicitHeight: content.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: AgentsUsage.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: AgentsUsage.hidePanel()

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

                // Horizontal bar row: label, proportional fill, value —
                // same visual language as VitalsPanel.qml's per-core bars.
                component BarRow: RowLayout {
                    Layout.fillWidth: true
                    spacing: 8
                    property string label
                    property real fraction: 0 // 0..1
                    property string value

                    Text {
                        text: parent.label
                        color: Theme.subtext0
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(10)
                        Layout.preferredWidth: 34
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: 8
                        radius: 4
                        color: Theme.surface2

                        Rectangle {
                            anchors.left: parent.left
                            anchors.top: parent.top
                            anchors.bottom: parent.bottom
                            width: parent.width * Math.max(0, Math.min(1, parent.parent.fraction))
                            radius: 4
                            color: Theme.blue
                        }
                    }

                    Text {
                        text: parent.value
                        color: Theme.text
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(10)
                        Layout.preferredWidth: 46
                        horizontalAlignment: Text.AlignRight
                    }
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 12

                    function fmtTokens(n) {
                        if (n >= 1e9) return (n / 1e9).toFixed(1) + "B";
                        if (n >= 1e6) return (n / 1e6).toFixed(1) + "M";
                        if (n >= 1e3) return (n / 1e3).toFixed(1) + "k";
                        return String(n);
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 10

                        Text {
                            text: "\u{F0AE2}" // nf-md-star_four_points
                            color: Theme.yellow
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(22)
                        }

                        ColumnLayout {
                            spacing: 0
                            Text {
                                text: AgentsUsage.source === "claude" ? "Claude Code" : "opencode"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(15)
                            }
                            Text {
                                text: "LOCAL USAGE"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                            }
                        }
                    }

                    component SourceTab: Rectangle {
                        property string label
                        property bool selected: false
                        signal activated()
                        Layout.fillWidth: true
                        implicitHeight: Config.px(26)
                        radius: 6
                        color: selected ? Theme.blue : Theme.surface1

                        Text {
                            anchors.centerIn: parent
                            text: parent.label
                            color: parent.selected ? Theme.crust : Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(11)
                            font.bold: parent.selected
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: parent.activated()
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 6

                        SourceTab { label: "Claude Code"; selected: AgentsUsage.source === "claude"; onActivated: AgentsUsage.source = "claude" }
                        SourceTab { label: "opencode"; selected: AgentsUsage.source === "opencode"; onActivated: AgentsUsage.source = "opencode" }
                    }

                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4
                        visible: AgentsUsage.tokensByDay.length > 0

                        SectionLabel { text: "TOKENS BY DAY" }

                        readonly property real maxDay: Math.max(1, ...AgentsUsage.tokensByDay.map(d => d.tokens))

                        Repeater {
                            model: AgentsUsage.tokensByDay
                            delegate: BarRow {
                                required property var modelData
                                label: modelData.day
                                fraction: modelData.tokens / parent.maxDay
                                value: content.fmtTokens(modelData.tokens)
                            }
                        }
                    }

                    Rectangle { Layout.fillWidth: true; height: 1; color: Theme.surface2 }

                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4
                        visible: AgentsUsage.tokensByModel.length > 0

                        SectionLabel { text: "TOKENS BY MODEL" }

                        readonly property real maxModel: Math.max(1, ...AgentsUsage.tokensByModel.map(m => m.tokens))

                        Repeater {
                            model: AgentsUsage.tokensByModel
                            delegate: BarRow {
                                required property var modelData
                                label: modelData.model
                                fraction: modelData.tokens / parent.maxModel
                                value: content.fmtTokens(modelData.tokens)
                            }
                        }
                    }
                }
            }
        }
    }
}
