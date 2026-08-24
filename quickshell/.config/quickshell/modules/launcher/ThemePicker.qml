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
    readonly property string repoWallpapers: Quickshell.env("HOME") + "/mydotfiles/Wallpapers"

    // Real wallpaper thumbnails read better than the abstract color-swatch
    // strips. A static map instead of scanning Wallpapers/<name>/ with a
    // `find` Process on every popup open — the file list only changes when
    // someone adds/removes a wallpaper, so there's nothing to discover at
    // runtime; hardcoding it also means the image never has to wait on a
    // subprocess round-trip before it can start loading.
    //
    // Three of the four Catppuccin flavors (latte/frappe/macchiato) have no
    // dedicated Wallpapers/<name>/ folder — same as theme/justfile's own
    // default_wallpaper map, kept in sync with it by hand.
    readonly property var thumbFor: ({
        mocha: repoWallpapers + "/DSC_0749-1.jpg",
        latte: repoWallpapers + "/DSC_0042.jpg",
        frappe: repoWallpapers + "/DSC_0194-1.jpg",
        macchiato: repoWallpapers + "/DSC_0515.JPG",
        nord: repoWallpapers + "/nord/preikestolen.jpg",
        gruvbox: repoWallpapers + "/gruvbox/valley-of-fire.jpg",
        dracula: repoWallpapers + "/dracula/base.png",
        tokyonight: repoWallpapers + "/tokyonight/gnome.png",
        rosepine: repoWallpapers + "/rosepine/maze.png",
        everforest: repoWallpapers + "/everforest/misty-pines.jpg",
        kanagawa: repoWallpapers + "/kanagawa/hokusai-tago-bay.jpg",
        "matte-black": repoWallpapers + "/matte-black/dark-marble.jpg",
        "osaka-jade": repoWallpapers + "/osaka-jade/bamboo-grove.jpg",
    })

    function thumbSource(name) {
        return "file://" + (root.thumbFor[name] ?? (root.repoTheme + "/swatches/" + name + ".png"));
    }

    LazyLoader {
        active: LauncherBus.themePickerVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.12)
            implicitWidth: Config.px(560)
            implicitHeight: Config.px(460)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            property string current: ""

            Process {
                id: readCurrent
                command: ["cat", root.repoTheme + "/.current"]
                stdout: StdioCollector {
                    onStreamFinished: {
                        win.current = this.text.trim();
                        const idx = root.order.indexOf(win.current);
                        if (idx >= 0) grid.currentIndex = idx;
                    }
                }
            }

            Component.onCompleted: {
                readCurrent.running = true;
                grid.forceActiveFocus();
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
                        font.pixelSize: Config.px(15)
                    }

                    GridView {
                        id: grid
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        cellWidth: 130
                        cellHeight: 110
                        model: root.order
                        focus: true
                        keyNavigationWraps: true

                        Keys.onReturnPressed: win.apply(root.order[grid.currentIndex])
                        Keys.onEnterPressed: win.apply(root.order[grid.currentIndex])
                        Keys.onEscapePressed: LauncherBus.themePickerVisible = false

                        delegate: Item {
                            required property var modelData
                            required property int index
                            width: grid.cellWidth
                            height: grid.cellHeight

                            readonly property bool isCurrent: modelData === win.current
                            readonly property bool isSelected: index === grid.currentIndex

                            Rectangle {
                                anchors.fill: parent
                                anchors.margins: 6
                                radius: 10
                                color: Theme.surface1
                                border.color: isSelected ? Theme.lavender : (isCurrent ? Theme.blue : "transparent")
                                border.width: 2

                                ColumnLayout {
                                    anchors.fill: parent
                                    anchors.margins: 8
                                    spacing: 6

                                    Image {
                                        Layout.fillWidth: true
                                        Layout.preferredHeight: 60
                                        source: root.thumbSource(modelData)
                                        fillMode: Image.PreserveAspectCrop
                                        asynchronous: true
                                    }

                                    Text {
                                        text: (root.displayNames[modelData] ?? modelData) + (isCurrent ? " ✓" : "")
                                        color: Theme.text
                                        font.family: Config.fontFamily
                                        font.pixelSize: Config.px(11)
                                        Layout.fillWidth: true
                                        horizontalAlignment: Text.AlignHCenter
                                        elide: Text.ElideRight
                                    }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    onClicked: {
                                        grid.currentIndex = index;
                                        win.apply(modelData);
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
