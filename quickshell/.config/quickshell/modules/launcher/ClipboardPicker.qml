import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.theme
import qs.config
import qs.services

// Replaces cliphist-picker.py (ALT+V). Rebuilt on qs.services.Cliphist
// (adapted from the community-standard services/Cliphist.qml pattern, see
// that file's header) instead of a custom Python list+thumbnail script.
//
// Image thumbnails: no ImageMagick pipeline. Same simplification the
// community's CliphistImage.qml uses — decode the entry once to a per-id
// cache file on demand and let QML's own Image do the (async, GPU-assisted)
// downscaling, instead of pre-generating a separate thumbnail file.
Scope {
    id: root

    LazyLoader {
        active: LauncherBus.clipboardVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.12)
            implicitWidth: 420
            implicitHeight: 480
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            Component.onCompleted: {
                Cliphist.refresh();
                search.forceActiveFocus();
            }

            readonly property var filtered: {
                const q = search.text.trim().toLowerCase();
                const list = Cliphist.entries;
                if (!q) return list;
                return list.filter(e => e.toLowerCase().includes(q));
            }

            function select(entry) {
                if (!entry) return;
                Cliphist.copy(entry);
                LauncherBus.clipboardVisible = false;
                search.text = "";
                list.currentIndex = 0;
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
                            text: "Clipboard History"
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.pixelSize: 12
                            font.bold: true
                        }

                        Item { Layout.fillWidth: true }

                        Text {
                            text: win.filtered.length + " items"
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
                            text: "Search clipboard history…"
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

                            Keys.onEscapePressed: LauncherBus.clipboardVisible = false
                            Keys.onDownPressed: list.incrementCurrentIndex()
                            Keys.onUpPressed: list.decrementCurrentIndex()
                            Keys.onReturnPressed: win.select(win.filtered[list.currentIndex])
                            Keys.onPressed: event => {
                                if (event.key === Qt.Key_Delete) {
                                    const entry = win.filtered[list.currentIndex];
                                    if (entry) Cliphist.deleteEntry(entry);
                                    event.accepted = true;
                                }
                            }
                        }
                    }

                    ListView {
                        id: list
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        model: win.filtered
                        currentIndex: 0
                        spacing: 3
                        reuseItems: true
                        boundsBehavior: Flickable.StopAtBounds

                        delegate: Rectangle {
                            id: delegateRoot
                            required property string modelData
                            required property int index
                            width: list.width
                            height: 46
                            radius: 8
                            color: index === list.currentIndex ? Theme.surface2 : "transparent"

                            readonly property bool isImage: Cliphist.entryIsImage(modelData)
                            readonly property string previewText: modelData.replace(/^\d+\t/, "")
                            readonly property string entryId: modelData.split("\t", 1)[0]

                            // Accent bar on the selected row, same visual
                            // language as the highlighted-row treatment
                            // elsewhere in this shell (e.g. GroupPill).
                            Rectangle {
                                visible: index === list.currentIndex
                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.bottom: parent.bottom
                                width: 3
                                radius: 2
                                color: Theme.blue
                            }

                            RowLayout {
                                anchors.fill: parent
                                anchors.leftMargin: 12
                                anchors.rightMargin: 10
                                spacing: 10

                                Rectangle {
                                    Layout.preferredWidth: 34
                                    Layout.preferredHeight: 34
                                    radius: 6
                                    color: delegateRoot.isImage ? "transparent" : Theme.surface1
                                    clip: true

                                    Loader {
                                        anchors.fill: parent
                                        active: delegateRoot.isImage
                                        sourceComponent: ClipboardThumb {
                                            entryId: delegateRoot.entryId
                                        }
                                    }

                                    // Text entries get a small monospace
                                    // glyph-free marker instead of a guessed
                                    // Nerd Font icon codepoint (avoids the
                                    // PUA-glyph-drops-silently issue seen
                                    // elsewhere in this repo) — just a
                                    // theme-colored dot for visual rhythm.
                                    Rectangle {
                                        visible: !delegateRoot.isImage
                                        anchors.centerIn: parent
                                        width: 8
                                        height: 8
                                        radius: 4
                                        color: Theme.overlay0
                                    }
                                }

                                Text {
                                    text: delegateRoot.previewText
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: 13
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: list.currentIndex = index
                                onClicked: win.select(modelData)
                            }
                        }
                    }
                }
            }
        }
    }
}
