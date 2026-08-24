import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import qs.theme
import qs.config
import qs.services
import qs.modules.bar

// Replaces `rofi -show window` (SUPER+ALT+Tab, hypr/.config/hypr/conf/
// keybindings.lua:42). Backed directly by Hypr.toplevels (Quickshell.
// Hyprland's live client list) instead of shelling to `hyprctl clients -j`.
//
// This machine's Hyprland build has a Lua-native dispatch IPC (hl.dispatch /
// hl.dsp.*, see hypr/.config/hypr/conf/keybindings.lua) instead of vanilla
// Hyprland's classic "dispatch <dispatcher> <args>" text protocol — the old
// "focuswindow address:<addr>" string is rejected with a Lua parse error.
// Confirmed live working syntax:
// `hyprctl dispatch 'hl.dsp.focus({window = "address:<addr>"})'`.
Scope {
    id: root

    LazyLoader {
        active: LauncherBus.windowSwitcherVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.15)
            implicitWidth: Config.px(480)
            implicitHeight: Config.px(360)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            Component.onCompleted: search.forceActiveFocus()

            readonly property var windows: Hypr.toplevels?.values ?? []
            readonly property var filtered: {
                const q = search.text.trim().toLowerCase();
                if (!q) return windows;
                return windows.filter(w => (w.title ?? "").toLowerCase().includes(q));
            }

            function focusWindow(w) {
                if (!w) return;
                Hypr.dispatch("hl.dsp.focus({window = \"address:" + w.address + "\"})");
                LauncherBus.windowSwitcherVisible = false;
                search.text = "";
                list.currentIndex = 0;
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

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: Config.px(36)
                        radius: 8
                        color: Theme.surface1

                        Text {
                            visible: search.text.length === 0
                            anchors.verticalCenter: parent.verticalCenter
                            anchors.left: parent.left
                            anchors.leftMargin: 10
                            text: "Switch window…"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(14)
                        }

                        TextInput {
                            id: search
                            anchors.fill: parent
                            anchors.leftMargin: 10
                            anchors.rightMargin: 10
                            verticalAlignment: TextInput.AlignVCenter
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(14)
                            focus: true
                            clip: true

                            Keys.onEscapePressed: LauncherBus.windowSwitcherVisible = false
                            Keys.onDownPressed: list.incrementCurrentIndex()
                            Keys.onUpPressed: list.decrementCurrentIndex()
                            Keys.onTabPressed: list.incrementCurrentIndex()
                            Keys.onReturnPressed: win.focusWindow(win.filtered[list.currentIndex])
                        }
                    }

                    ListView {
                        id: list
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        model: win.filtered
                        currentIndex: 0

                        delegate: Rectangle {
                            id: delegateRoot
                            required property var modelData
                            required property int index
                            width: list.width
                            height: 36
                            radius: 6
                            color: index === list.currentIndex ? Theme.surface2 : "transparent"

                            // Native icon lookup, same approach as
                            // Launcher.qml: no custom script, just
                            // DesktopEntries.byId() (keyed by the app's
                            // desktop-file id, which usually matches the
                            // Wayland appId Hyprland reports) + iconPath().
                            // byId() wants an exact desktop-entry id, which
                            // doesn't always match the raw Wayland appId
                            // Hyprland reports; heuristicLookup() is
                            // Quickshell's native fuzzy fallback for this
                            // exact mismatch, same pattern used by
                            // caelestia-dots/shell, DankMaterialShell, and
                            // other community Quickshell configs.
                            readonly property var desktopEntry: modelData.appId ? (DesktopEntries.byId(modelData.appId) ?? DesktopEntries.heuristicLookup(modelData.appId)) : null
                            readonly property string iconSource: desktopEntry?.icon ? Quickshell.iconPath(desktopEntry.icon, true) : ""

                            RowLayout {
                                anchors.fill: parent
                                anchors.leftMargin: 10
                                anchors.rightMargin: 10
                                spacing: 8

                                Text {
                                    text: modelData.workspace?.name ?? ""
                                    color: Theme.overlay0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(12)
                                    Layout.preferredWidth: 30
                                }

                                IconImage {
                                    visible: delegateRoot.iconSource !== ""
                                    source: delegateRoot.iconSource
                                    implicitSize: 18
                                    asynchronous: true
                                }

                                Text {
                                    text: WindowRewriteRules.resolve(modelData.title ?? "").label
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(13)
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: list.currentIndex = index
                                onClicked: win.focusWindow(modelData)
                            }
                        }
                    }
                }
            }
        }
    }
}
