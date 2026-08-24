import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.theme
import qs.config
import qs.services

// Lets you change the lock screen's wallpaper/blur without hand-editing
// services/Lock.qml. Same GridView-of-thumbnails pattern as ThemePicker.qml,
// scanning ~/mydotfiles/Wallpapers directly (find, once per open — same
// on-demand-not-polled convention as Network.qml's refreshIpInfo). Writes go
// through Settings.qml (Quickshell.Io.JsonAdapter), which Lock.qml/
// LockScreen.qml read live.
Scope {
    id: root

    readonly property string wallpaperDir: Quickshell.env("HOME") + "/mydotfiles/Wallpapers"

    LazyLoader {
        active: LauncherBus.lockAppearanceVisible

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

            property var wallpapers: []

            Process {
                id: scanWallpapers
                command: ["find", root.wallpaperDir, "-maxdepth", "1", "-type", "f",
                    "(", "-iname", "*.jpg", "-o", "-iname", "*.jpeg", "-o", "-iname", "*.png", ")"]
                stdout: StdioCollector {
                    onStreamFinished: {
                        win.wallpapers = this.text.trim().split("\n").filter(l => l.length > 0).sort();
                        const idx = win.wallpapers.indexOf(Settings.lockWallpaper);
                        if (idx >= 0) grid.currentIndex = idx;
                    }
                }
            }

            Component.onCompleted: {
                scanWallpapers.running = true;
                grid.forceActiveFocus();
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: LauncherBus.lockAppearanceVisible = false

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    Text {
                        text: "Lock Screen Appearance"
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
                        cellHeight: 90
                        model: win.wallpapers
                        focus: true
                        keyNavigationWraps: true

                        Keys.onReturnPressed: Settings.lockWallpaper = win.wallpapers[grid.currentIndex]
                        Keys.onEnterPressed: Settings.lockWallpaper = win.wallpapers[grid.currentIndex]
                        Keys.onEscapePressed: LauncherBus.lockAppearanceVisible = false

                        delegate: Item {
                            required property string modelData
                            required property int index
                            width: grid.cellWidth
                            height: grid.cellHeight

                            readonly property bool isCurrent: modelData === Settings.lockWallpaper
                            readonly property bool isSelected: index === grid.currentIndex

                            Rectangle {
                                anchors.fill: parent
                                anchors.margins: 6
                                radius: 10
                                color: Theme.surface1
                                border.color: isSelected ? Theme.lavender : (isCurrent ? Theme.blue : "transparent")
                                border.width: 2
                                clip: true

                                Image {
                                    anchors.fill: parent
                                    source: "file://" + modelData
                                    fillMode: Image.PreserveAspectCrop
                                    asynchronous: true
                                    cache: false
                                }

                                Text {
                                    visible: isCurrent
                                    anchors.top: parent.top
                                    anchors.right: parent.right
                                    anchors.margins: 4
                                    text: "✓"
                                    color: Theme.blue
                                    font.family: Config.fontFamily
                                    font.bold: true
                                    font.pixelSize: Config.px(13)
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    onClicked: {
                                        grid.currentIndex = index;
                                        Settings.lockWallpaper = modelData;
                                    }
                                }
                            }
                        }
                    }

                    // Blur slider — same hand-rolled draggable-handle
                    // control as BrightnessPanel.qml (no QtQuick.Controls
                    // installed on this system).
                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4

                        RowLayout {
                            Layout.fillWidth: true
                            Text {
                                text: "Blur"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                                font.bold: true
                                Layout.fillWidth: true
                            }
                            Text {
                                text: Math.round(Settings.lockBlur)
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(11)
                            }
                        }

                        Item {
                            id: slider
                            Layout.fillWidth: true
                            implicitHeight: Config.px(20)

                            readonly property real ratio: Math.max(0, Math.min(1, Settings.lockBlur / 128))

                            function setFromX(x) {
                                const clamped = Math.max(0, Math.min(1, x / slider.width));
                                Settings.lockBlur = Math.round(clamped * 128);
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
                    }
                }
            }
        }
    }
}
