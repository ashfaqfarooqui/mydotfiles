import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Emoji search/picker, modeled directly on ClipboardPicker.qml's structure
// (search TextInput + filtering + HyprlandFocusGrab dismiss + keyboard nav)
// but a GridView of emoji cells instead of a ListView of text rows.
//
// Dataset: modules/launcher/emoji-data.json, vendored from muan's
// unicode-emoji-json (MIT) — [{emoji, name}, ...], ~1900 entries.
//
// Selection: unlike ClipboardPicker (copies to clipboard only), this types
// the emoji directly into whatever window regains focus once the picker
// closes, via `wtype`. Omarchy does a clipboard-copy-then-paste-synthesis
// dance for this; skipped here since wtype can type arbitrary UTF-8 text
// on its own — one shell call, no clipboard round-trip. A tiny sleep before
// wtype gives Hyprland time to hand focus back to the previous window
// after this panel's HyprlandFocusGrab releases it.
Scope {
    id: root

    // Loaded eagerly (not inside the LazyLoader below) — it's a small
    // (~80KB) static file, and loading it only once the panel's LazyLoader
    // activates raced the panel's first render against the async file
    // read, showing "0 results" until something else nudged a re-evaluation.
    FileView {
        id: dataFile
        path: Qt.resolvedUrl("./emoji-data.json")
    }

    LazyLoader {
        active: LauncherBus.emojiVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.12)
            implicitWidth: 480
            implicitHeight: 420
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            readonly property var allEmoji: {
                try {
                    return JSON.parse(dataFile.text());
                } catch (e) {
                    return [];
                }
            }

            readonly property var filtered: {
                const q = search.text.trim().toLowerCase();
                if (!q) return win.allEmoji;
                return win.allEmoji.filter(e => e.name.toLowerCase().includes(q));
            }

            Component.onCompleted: search.forceActiveFocus()

            HyprlandFocusGrab {
                active: LauncherBus.emojiVisible
                windows: [win]
                onCleared: LauncherBus.emojiVisible = false
            }

            function select(entry) {
                if (!entry) return;
                LauncherBus.emojiVisible = false;
                search.text = "";
                grid.currentIndex = 0;
                Quickshell.execDetached(["sh", "-c", "sleep 0.05; wtype " + JSON.stringify(entry.emoji)]);
            }

            Rectangle {
                anchors.fill: parent
                radius: 14
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
                            text: "Emoji"
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.pixelSize: 12
                            font.bold: true
                        }

                        Item { Layout.fillWidth: true }

                        Text {
                            text: win.filtered.length + " results"
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
                            text: "Search emoji…"
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

                            Keys.onEscapePressed: LauncherBus.emojiVisible = false
                            Keys.onReturnPressed: win.select(win.filtered[grid.currentIndex])
                            Keys.onPressed: event => {
                                const columns = Math.max(1, Math.floor(grid.width / grid.cellWidth));
                                if (event.key === Qt.Key_Right) { grid.currentIndex = Math.min(win.filtered.length - 1, grid.currentIndex + 1); event.accepted = true; }
                                else if (event.key === Qt.Key_Left) { grid.currentIndex = Math.max(0, grid.currentIndex - 1); event.accepted = true; }
                                else if (event.key === Qt.Key_Down) { grid.currentIndex = Math.min(win.filtered.length - 1, grid.currentIndex + columns); event.accepted = true; }
                                else if (event.key === Qt.Key_Up) { grid.currentIndex = Math.max(0, grid.currentIndex - columns); event.accepted = true; }
                            }
                            onTextChanged: grid.currentIndex = 0
                        }
                    }

                    GridView {
                        id: grid
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        model: win.filtered
                        currentIndex: 0
                        cellWidth: 52
                        cellHeight: 52
                        boundsBehavior: Flickable.StopAtBounds

                        delegate: Rectangle {
                            id: cell
                            required property var modelData
                            required property int index
                            width: grid.cellWidth - 4
                            height: grid.cellHeight - 4
                            radius: 8
                            color: index === grid.currentIndex ? Theme.surface2 : "transparent"

                            Text {
                                anchors.centerIn: parent
                                text: cell.modelData.emoji
                                font.pixelSize: 22
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: grid.currentIndex = index
                                onClicked: win.select(cell.modelData)
                            }
                        }
                    }
                }
            }
        }
    }
}
