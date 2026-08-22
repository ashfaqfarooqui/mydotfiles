import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.theme
import qs.config
import qs.services

// Unifies three previously-separate power UIs into one menu (per the plan's
// "Correction" section): swaync's buttons-grid -> wlogout button, waybar's
// custom/power -> power-menu.sh (rofi, Lock/Shutdown/Reboot/Suspend/
// Hibernate/Logout), and the Hyprland Session submap (SUPER+Escape:
// l=lock, e=exit, s=suspend, r=reboot, p=poweroff — missing Hibernate,
// which power-menu.sh has). This is the union of all six actions.
//
// Filterable list + arrow-key nav, same pattern as Cheatsheet/Launcher's
// rows, rather than a click-only grid — keyboard-only operation matches
// how the rest of Phase 3's popups behave.
//
// Icons: the icons originally reused here (ControlCenter.qml's
// \u{F0373}/\u{F0474}) never actually rendered. Those are Nerd Fonts'
// newer Material Design Icons set, which live in the Supplementary PUA-A
// plane (U+F0000+) — checking the installed font's own charset table
// (`fc-query /usr/share/fonts/TTF/JetBrainsMonoNerdFont-Regular.ttf`)
// shows that plane isn't covered by this font at all. The classic Font
// Awesome range (U+F000-U+F5FF) *is* fully covered by this font, confirmed
// via the same fc-query charset dump, and matches the codepoint the bar's
// PowerButton.qml already uses successfully (U+F011). All six icons below
// use that classic range instead — verified landing correctly via a raw
// codepoint dump after writing, not just visually, since pasted glyphs
// have silently dropped in this environment before.
Scope {
    id: root

    // entries[i].run is a plain JS function *value* stored in an object
    // literal — safe, unlike the property-var-assigned-a-call-expression
    // bug this file used to have (see the plan file's incident writeup):
    // storing a function here never calls it; only entries[i].run() does,
    // from inside an actual event handler below.
    readonly property var entries: [
        { label: "Lock", icon: "", needsConfirm: false, run: () => win.act(["hyprlock"]) },
        { label: "Suspend", icon: "", needsConfirm: true, run: () => win.act(["systemctl", "suspend"]) },
        { label: "Hibernate", icon: "", needsConfirm: true, run: () => win.act(["systemctl", "hibernate"]) },
        { label: "Reboot", icon: "", needsConfirm: true, run: () => win.act(["systemctl", "reboot"]) },
        { label: "Poweroff", icon: "", needsConfirm: true, run: () => win.act(["systemctl", "poweroff"]) },
        // This machine's Hyprland build uses a Lua-native dispatch IPC
        // (hl.dispatch/hl.dsp.*) instead of vanilla Hyprland's classic
        // dispatch text protocol — plain "dispatch exit 0" is rejected.
        // Confirmed live: hl.dsp.exit exists and takes no args.
        { label: "Logout", icon: "", needsConfirm: true, run: () => win.act(["hyprctl", "dispatch", "hl.dsp.exit()"]) },
    ]

    LazyLoader {
        active: LauncherBus.powerMenuVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.2)
            implicitWidth: 320
            implicitHeight: 340
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            function act(command) {
                Quickshell.execDetached(command);
                LauncherBus.powerMenuVisible = false;
            }

            readonly property var filtered: {
                const q = search.text.trim().toLowerCase();
                if (!q) return root.entries;
                return root.entries.filter(e => e.label.toLowerCase().includes(q));
            }

            // Confirm-arm state lives here (not per-row) so moving the
            // selection or filtering always disarms — a destructive
            // action can only fire from two Enters/clicks on the SAME
            // row in a row, never a stray leftover arm from a different
            // item. Independent second line of defense on top of the
            // property/signal fix above: even a correctly-wired
            // accidental activation on Reboot/Poweroff/Logout/Suspend/
            // Hibernate shouldn't be able to act alone.
            property int armedIndex: -1

            function disarm() {
                armedIndex = -1;
                disarmTimer.stop();
            }

            function activate(idx) {
                const entry = filtered[idx];
                if (!entry) return;
                if (!entry.needsConfirm) {
                    entry.run();
                    return;
                }
                if (armedIndex === idx) {
                    win.disarm();
                    entry.run();
                } else {
                    armedIndex = idx;
                    disarmTimer.restart();
                }
            }

            Timer {
                id: disarmTimer
                interval: 2500
                onTriggered: win.armedIndex = -1
            }

            Component.onCompleted: search.forceActiveFocus()

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

                    Text {
                        text: "Power"
                        color: Theme.subtext1
                        font.family: Config.fontFamily
                        font.pixelSize: 12
                        font.bold: true
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
                            text: "Search…"
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

                            onTextChanged: {
                                win.disarm();
                                list.currentIndex = 0;
                            }

                            Keys.onEscapePressed: LauncherBus.powerMenuVisible = false
                            Keys.onDownPressed: {
                                win.disarm();
                                list.incrementCurrentIndex();
                            }
                            Keys.onUpPressed: {
                                win.disarm();
                                list.decrementCurrentIndex();
                            }
                            Keys.onReturnPressed: win.activate(list.currentIndex)
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
                            required property var modelData
                            required property int index
                            width: list.width
                            implicitHeight: 40
                            radius: 8
                            readonly property bool armed: win.armedIndex === index
                            color: armed ? Theme.red : (index === list.currentIndex ? Theme.surface2 : "transparent")

                            RowLayout {
                                anchors.fill: parent
                                anchors.leftMargin: 12
                                anchors.rightMargin: 12
                                spacing: 10

                                Text {
                                    text: delegateRoot.modelData.icon
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: 16
                                    Layout.preferredWidth: 20
                                }

                                Text {
                                    text: delegateRoot.armed ? "Press again to confirm" : delegateRoot.modelData.label
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: 14
                                    font.bold: delegateRoot.armed
                                    Layout.fillWidth: true
                                }
                            }

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: list.currentIndex = index
                                onClicked: win.activate(index)
                            }
                        }
                    }
                }
            }
        }
    }
}
