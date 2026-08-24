import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Opened by SUPER+CTRL+C (hypr/.config/hypr/conf/keybindings.lua) via
// `quickshell ipc call capture toggle`. List-popup pattern modeled on
// ThemePicker.qml/WindowSwitcher.qml — PanelWindow + keyCatcher Item for
// Keys, HyprlandFocusGrab for click-outside dismiss (same mechanism as
// NetworkPanel.qml/BluetoothPanel.qml).
Scope {
    id: root

    readonly property var actions: [
        { label: "Screenshot — Region", run: () => Capture.screenshot("region") },
        { label: "Screenshot — Window", run: () => Capture.screenshot("window") },
        { label: "Screenshot — Full Screen", run: () => Capture.screenshot("output") },
        {
            label: Capture.recording ? "Stop Recording" : "Start Recording",
            run: () => Capture.recording ? Capture.stopRecording() : Capture.startRecording(),
        },
        { label: "OCR Region", run: () => Capture.ocrRegion() },
    ]

    LazyLoader {
        active: Capture.menuVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.15)
            implicitWidth: 320
            implicitHeight: list.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: Capture.menuVisible
                windows: [win]
                onCleared: Capture.menuVisible = false
            }

            Component.onCompleted: keyCatcher.forceActiveFocus()

            Rectangle {
                id: keyCatcher
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Capture.menuVisible = false
                Keys.onDownPressed: list.incrementCurrentIndex()
                Keys.onUpPressed: list.decrementCurrentIndex()
                Keys.onReturnPressed: root.actions[list.currentIndex].run()
                Keys.onEnterPressed: root.actions[list.currentIndex].run()

                ColumnLayout {
                    id: list
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 4

                    property int currentIndex: 0

                    function incrementCurrentIndex() {
                        currentIndex = (currentIndex + 1) % root.actions.length;
                    }
                    function decrementCurrentIndex() {
                        currentIndex = (currentIndex - 1 + root.actions.length) % root.actions.length;
                    }

                    Repeater {
                        model: root.actions

                        delegate: Rectangle {
                            required property var modelData
                            required property int index
                            Layout.fillWidth: true
                            implicitHeight: 36
                            radius: 6
                            color: index === list.currentIndex ? Theme.surface2 : "transparent"

                            Text {
                                anchors.fill: parent
                                anchors.leftMargin: 10
                                verticalAlignment: Text.AlignVCenter
                                text: modelData.label
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: 13
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: list.currentIndex = index
                                onClicked: modelData.run()
                            }
                        }
                    }
                }
            }
        }
    }
}
