import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.theme
import qs.config
import qs.services

// Replaces hypr/.config/hypr/scripts/theme-picker.sh (SUPER+CTRL+SHIFT+
// SPACE). Same theme list/order/display names and swatch images
// (theme/swatches/*.png), same apply mechanism (`just -f theme/justfile
// apply <flavor>`) and same "(current)" marker sourced from theme/.current
// — just a grid instead of a rofi -show-icons dmenu.
Scope {
    id: root

    readonly property var order: ["mocha", "latte", "frappe", "macchiato", "nord", "gruvbox", "dracula", "tokyonight", "rosepine", "everforest", "kanagawa", "matte-black", "osaka-jade"]
    readonly property var displayNames: ({
        mocha: "Catppuccin Mocha",
        latte: "Catppuccin Latte",
        frappe: "Catppuccin Frappé",
        macchiato: "Catppuccin Macchiato",
        nord: "Nord",
        gruvbox: "Gruvbox",
        dracula: "Dracula",
        tokyonight: "Tokyo Night",
        rosepine: "Rosé Pine",
        everforest: "Everforest",
        kanagawa: "Kanagawa",
        "matte-black": "Matte Black",
        "osaka-jade": "Osaka Jade",
    })

    readonly property string repoTheme: Quickshell.env("HOME") + "/mydotfiles/theme"

    LazyLoader {
        active: LauncherBus.themePickerVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.12)
            implicitWidth: 560
            implicitHeight: 460
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            property string current: ""

            Process {
                id: readCurrent
                command: ["cat", root.repoTheme + "/.current"]
                stdout: StdioCollector {
                    onStreamFinished: win.current = this.text.trim()
                }
            }

            Component.onCompleted: {
                readCurrent.running = true;
                keyCatcher.forceActiveFocus();
            }

            function apply(flavor) {
                Quickshell.execDetached(["just", "-f", root.repoTheme + "/justfile", "-d", root.repoTheme, "apply", flavor]);
                win.current = flavor;
                LauncherBus.themePickerVisible = false;
            }

            Rectangle {
                id: keyCatcher
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: LauncherBus.themePickerVisible = false

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    Text {
                        text: "Theme"
                        color: Theme.text
                        font.family: Config.fontFamily
                        font.bold: true
                        font.pixelSize: 15
                    }

                    GridView {
                        id: grid
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        cellWidth: 130
                        cellHeight: 110
                        model: root.order

                        delegate: Item {
                            required property var modelData
                            width: grid.cellWidth
                            height: grid.cellHeight

                            readonly property bool isCurrent: modelData === win.current

                            Rectangle {
                                anchors.fill: parent
                                anchors.margins: 6
                                radius: 10
                                color: Theme.surface1
                                border.color: isCurrent ? Theme.blue : "transparent"
                                border.width: 2

                                ColumnLayout {
                                    anchors.fill: parent
                                    anchors.margins: 8
                                    spacing: 6

                                    Image {
                                        Layout.fillWidth: true
                                        Layout.preferredHeight: 60
                                        source: "file://" + root.repoTheme + "/swatches/" + modelData + ".png"
                                        fillMode: Image.PreserveAspectCrop
                                    }

                                    Text {
                                        text: root.displayNames[modelData] ?? modelData
                                        color: Theme.text
                                        font.family: Config.fontFamily
                                        font.pixelSize: 11
                                        Layout.fillWidth: true
                                        horizontalAlignment: Text.AlignHCenter
                                        elide: Text.ElideRight
                                    }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    onClicked: win.apply(modelData)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
