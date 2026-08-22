import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.theme
import qs.config
import qs.services

// Replaces keybind-help.sh (SUPER+/). That script already reads
// `hyprctl binds -j` and formats it in bash/jq; this ports the same
// modmask-decode / key-format / submap-grouping logic to JS so the
// cheatsheet is a plain read-only searchable list instead of a rofi dmenu.
Scope {
    id: root

    function decodeModmask(mask) {
        const parts = [];
        if (mask & 64) parts.push("SUPER");
        if (mask & 1) parts.push("SHIFT");
        if (mask & 4) parts.push("CTRL");
        if (mask & 8) parts.push("ALT");
        return parts.join("+");
    }

    function formatKey(key) {
        switch (key) {
            case "return": return "Return";
            case "escape": return "Esc";
            case "space": return "Space";
            case "tab": return "Tab";
            case "comma": return ",";
            case "period": return ".";
            case "slash": return "/";
            case "mouse_down": return "ScrollDown";
            case "mouse_up": return "ScrollUp";
            default: return key;
        }
    }

    LazyLoader {
        active: LauncherBus.cheatsheetVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.08)
            implicitWidth: 560
            implicitHeight: 600
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            property var entries: []

            Process {
                id: scan
                command: ["hyprctl", "binds", "-j"]
                stdout: StdioCollector {
                    onStreamFinished: {
                        try {
                            const raw = JSON.parse(this.text);
                            win.entries = raw
                                .filter(b => b.has_description && b.description !== "")
                                .map(b => ({
                                    section: b.submap === "" ? "GLOBAL" : b.submap,
                                    combo: [root.decodeModmask(b.modmask), root.formatKey(b.key)].filter(s => s).join(" + "),
                                    description: b.description,
                                }));
                        } catch (e) {
                            win.entries = [];
                        }
                    }
                }
            }

            Component.onCompleted: {
                scan.running = true;
                search.forceActiveFocus();
            }

            readonly property var filtered: {
                const q = search.text.trim().toLowerCase();
                if (!q) return entries;
                return entries.filter(e => e.description.toLowerCase().includes(q) || e.combo.toLowerCase().includes(q));
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            text: "Keybinds"
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.pixelSize: 12
                            font.bold: true
                        }

                        Item { Layout.fillWidth: true }

                        Text {
                            text: win.filtered.length + " binds"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 11
                        }
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: 38
                        radius: 8
                        color: Theme.surface1
                        border.color: search.activeFocus ? Theme.blue : "transparent"
                        border.width: 1

                        Text {
                            visible: search.text.length === 0
                            anchors.verticalCenter: parent.verticalCenter
                            anchors.left: parent.left
                            anchors.leftMargin: 10
                            text: "Search keybinds…"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 14
                        }

                        TextInput {
                            id: search
                            anchors.fill: parent
                            anchors.leftMargin: 10
                            anchors.rightMargin: 10
                            verticalAlignment: TextInput.AlignVCenter
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: 14
                            focus: true
                            clip: true

                            Keys.onEscapePressed: LauncherBus.cheatsheetVisible = false
                        }
                    }

                    ListView {
                        id: list
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        model: win.filtered
                        reuseItems: true
                        boundsBehavior: Flickable.StopAtBounds
                        spacing: 2

                        delegate: RowLayout {
                            required property var modelData
                            width: list.width
                            height: 28
                            spacing: 10

                            Text {
                                text: modelData.section
                                color: Theme.mauve
                                font.family: Config.fontFamily
                                font.pixelSize: 10
                                Layout.preferredWidth: 70
                                elide: Text.ElideRight
                            }

                            Text {
                                text: modelData.combo
                                color: Theme.blue
                                font.family: Config.fontFamily
                                font.pixelSize: 12
                                Layout.preferredWidth: 160
                                elide: Text.ElideRight
                            }

                            Text {
                                text: modelData.description
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: 12
                                Layout.fillWidth: true
                                elide: Text.ElideRight
                            }
                        }
                    }
                }
            }
        }
    }
}
