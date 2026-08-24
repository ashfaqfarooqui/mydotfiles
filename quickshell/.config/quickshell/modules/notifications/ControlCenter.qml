import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import Quickshell.Hyprland
import Quickshell.Services.Pipewire
import Quickshell.Networking as QN
import Quickshell.Bluetooth as QB
import qs.theme
import qs.config
import qs.services

// Replaces swaync's control-center panel (swaync/.config/swaync/config.json:
// control-center-width 400, height 850, margins top/bottom 10, right 10,
// left 0, anchored top-right). Icon codepoints below are \u{XXXXX} escapes
// (outside the BMP, so \uXXXX 4-hex form doesn't cover them) extracted
// byte-exact from that same config.json and cross-checked against the
// installed JetBrainsMono Nerd Font's cmap — see Phase 1's icon-drop
// postmortem for why these are never hand-pasted.
Scope {
    id: root

    LazyLoader {
        active: Notifications.controlCenterVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors {
                top: true
                right: true
            }
            margins {
                top: Config.barHeight + 4
                right: 10
            }
            implicitWidth: Config.px(400)
            // Content-fit height, same pattern as NetworkPanel.qml/
            // BluetoothPanel.qml, instead of anchoring bottom and stretching
            // the panel across the whole screen — a short notification list
            // used to leave a huge mostly-empty panel hanging off the bar.
            // 3/4 of the screen is a hard cap for when there's genuinely a
            // lot of content (many grouped notifications + volume sliders).
            implicitHeight: Math.min(win.screen.height * 0.75, rootColumn.implicitHeight + 28)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                // Same click-outside-to-close mechanism as
                // NetworkPanel.qml/BluetoothPanel.qml.
                active: Notifications.controlCenterVisible
                windows: [win]
                onCleared: Notifications.hideControlCenter()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                // PanelWindow's layershell surface isn't a QtQuick Item, so
                // Keys can't attach there directly — an Item with
                // focus: true is required to receive routed key events (same
                // pattern as NetworkPanel.qml/ThemePicker.qml's keyCatcher).
                focus: true

                Keys.onEscapePressed: Notifications.hideControlCenter()

                ColumnLayout {
                    id: rootColumn
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 12

                    // Small uppercase overline label used above sections
                    // that don't already carry their own inline label (DND's
                    // row already says "Do Not Disturb", so it's left alone).
                    component SectionHeader: Text {
                        property string label
                        text: label
                        color: Theme.subtext0
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(10)
                        font.capitalization: Font.AllUppercase
                        font.letterSpacing: 1
                        Layout.topMargin: 4
                    }

                    // Unified card background for grouping a section's rows
                    // together — same radius/fill as the notification
                    // history cards below, so the whole panel reads as one
                    // card language instead of several ad hoc widget styles.
                    component SectionCard: Rectangle {
                        id: card
                        default property alias content: inner.data
                        property alias contentSpacing: inner.spacing
                        Layout.fillWidth: true
                        implicitHeight: inner.implicitHeight + 16
                        radius: 8
                        color: Theme.surface1

                        ColumnLayout {
                            id: inner
                            anchors.fill: parent
                            anchors.margins: 8
                            spacing: 6
                        }
                    }

                    // --- Title bar ---
                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "Notification Center"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(15)
                            Layout.fillWidth: true
                        }

                        Text {
                            text: "\u{F01B4}"
                            color: Theme.subtext0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(16)

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Notifications.clearAll()
                            }
                        }
                    }

                    // --- Quick action grid ---
                    SectionHeader { label: "Quick Actions" }

                    GridLayout {
                        Layout.fillWidth: true
                        columns: 4
                        columnSpacing: 8
                        rowSpacing: 8

                        component GridButton: Rectangle {
                            property string icon
                            // Reflects on/off state for toggle-style buttons
                            // (Wi-Fi/Bluetooth below); momentary-action
                            // buttons (lock/screenshot/wlogout) just leave
                            // this false.
                            property bool active: false
                            // A real signal, not `property var onActivated`
                            // assigned a call expression: that form is a
                            // QML property BINDING, whose right-hand side
                            // is evaluated immediately when the binding is
                            // established (i.e. the instant this component
                            // loads), not deferred until click — found via
                            // the identical bug in PowerMenuUI.qml, which
                            // fired every action (including a real
                            // poweroff) at popup-open time with zero
                            // clicks. `signal activated()` +
                            // `onActivated:` on a real signal is a genuine
                            // handler, evaluated only on emission.
                            signal activated()
                            Layout.fillWidth: true
                            Layout.preferredHeight: 44
                            radius: 8
                            color: active ? Theme.blue : Theme.surface1

                            Text {
                                anchors.centerIn: parent
                                text: parent.icon
                                color: parent.active ? Theme.crust : Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(16)
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: parent.activated()
                            }
                        }

                        GridButton {
                            // Was \u{F0373} (md-minecraft — a leftover
                            // mis-transcription, not an icon-drop like the
                            // ones documented above; verified correct
                            // codepoint against the installed font's cmap).
                            icon: "\u{F033E}"
                            onActivated: Quickshell.execDetached(["hyprlock"])
                        }
                        GridButton {
                            // md-crop — region/selection screenshot.
                            icon: "\u{F019E}"
                            onActivated: Capture.screenshot("region")
                        }
                        GridButton {
                            // md-monitor_screenshot — full-output screenshot
                            // (previously on the region button by mistake).
                            icon: "\u{F0E51}"
                            onActivated: Capture.screenshot("output")
                        }
                        GridButton {
                            // md-logout (was \u{F0474} = md-school).
                            icon: "\u{F0343}"
                            onActivated: Quickshell.execDetached(["wlogout"])
                        }
                        GridButton {
                            icon: "\u{F05A9}"
                            active: QN.Networking.wifiEnabled
                            onActivated: QN.Networking.wifiEnabled = !QN.Networking.wifiEnabled
                        }
                        GridButton {
                            icon: "\u{F00AF}"
                            active: QB.Bluetooth.defaultAdapter?.enabled ?? false
                            onActivated: {
                                if (QB.Bluetooth.defaultAdapter) QB.Bluetooth.defaultAdapter.enabled = !QB.Bluetooth.defaultAdapter.enabled;
                            }
                        }
                    }

                    // --- DND toggle ---
                    SectionCard {
                        RowLayout {
                            Layout.fillWidth: true

                            Text {
                                text: "Do Not Disturb"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(13)
                                Layout.fillWidth: true
                            }

                            Rectangle {
                                width: 40
                                height: 22
                                radius: 11
                                color: Notifications.dndEnabled ? Theme.blue : Theme.surface2

                                Rectangle {
                                    width: 18
                                    height: 18
                                    radius: 9
                                    color: Theme.text
                                    anchors.verticalCenter: parent.verticalCenter
                                    x: Notifications.dndEnabled ? parent.width - width - 2 : 2
                                    Behavior on x { NumberAnimation { duration: 120 } }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    onClicked: Notifications.toggleDnd()
                                }
                            }
                        }
                    }

                    // --- Mpris ---
                    SectionCard {
                        id: mprisCard
                        visible: Mpris.activePlayer !== null

                        property real currentPosition: Mpris.activePlayer?.position ?? 0

                        // Only exists while this SectionCard's LazyLoader-
                        // gated ancestor PanelWindow is actually loaded (the
                        // whole control center is behind `LazyLoader { active:
                        // Notifications.controlCenterVisible }`), so this
                        // timer never runs while the panel is closed.
                        Timer {
                            interval: 1000
                            running: Mpris.activePlayer?.isPlaying ?? false
                            repeat: true
                            onTriggered: mprisCard.currentPosition = Mpris.activePlayer?.position ?? 0
                        }

                        Connections {
                            target: Mpris
                            function onActivePlayerChanged() {
                                mprisCard.currentPosition = Mpris.activePlayer?.position ?? 0;
                            }
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 8

                            Rectangle {
                                Layout.preferredWidth: 40
                                Layout.preferredHeight: 40
                                radius: 6
                                color: Theme.surface2
                                clip: true

                                Image {
                                    anchors.fill: parent
                                    source: Mpris.activePlayer?.trackArtUrl ?? ""
                                    visible: (Mpris.activePlayer?.trackArtUrl ?? "") !== ""
                                    fillMode: Image.PreserveAspectCrop
                                }

                                Text {
                                    anchors.centerIn: parent
                                    visible: (Mpris.activePlayer?.trackArtUrl ?? "") === ""
                                    text: "♫"
                                    color: Theme.overlay0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(16)
                                }
                            }

                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 4

                                Text {
                                    text: Mpris.activePlayer?.trackTitle ?? ""
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.bold: true
                                    font.pixelSize: Config.px(12)
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }

                                Text {
                                    text: Mpris.activePlayer?.trackArtist ?? ""
                                    color: Theme.subtext0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(11)
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }

                                Rectangle {
                                    visible: (Mpris.activePlayer?.positionSupported ?? false) && (Mpris.activePlayer?.lengthSupported ?? false) && (Mpris.activePlayer?.length ?? 0) > 0
                                    Layout.fillWidth: true
                                    height: 3
                                    radius: 1.5
                                    color: Theme.surface2

                                    Rectangle {
                                        readonly property real fraction: (Mpris.activePlayer?.length ?? 0) > 0 ? Math.min(1, mprisCard.currentPosition / Mpris.activePlayer.length) : 0
                                        width: parent.width * fraction
                                        height: parent.height
                                        radius: 1.5
                                        color: Theme.blue
                                    }
                                }
                            }

                            RowLayout {
                                spacing: 4

                                Text {
                                    text: "\u{F04AE}"
                                    color: Mpris.activePlayer?.canGoPrevious ? Theme.text : Theme.overlay0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(13)

                                    MouseArea {
                                        anchors.fill: parent
                                        enabled: Mpris.activePlayer?.canGoPrevious ?? false
                                        onClicked: Mpris.previous()
                                    }
                                }

                                Text {
                                    text: Mpris.activePlayer?.isPlaying ? "\u{F03E4}" : "\u{F040A}"
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(15)

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: Mpris.playPause()
                                    }
                                }

                                Text {
                                    text: "\u{F04AD}"
                                    color: Mpris.activePlayer?.canGoNext ? Theme.text : Theme.overlay0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(13)

                                    MouseArea {
                                        anchors.fill: parent
                                        enabled: Mpris.activePlayer?.canGoNext ?? false
                                        onClicked: Mpris.next()
                                    }
                                }
                            }
                        }
                    }

                    // --- Per-app volume sliders ---
                    SectionHeader { label: "Applications"; visible: volumeSection.visible }

                    SectionCard {
                        id: volumeSection
                        visible: playbackNodes.length > 0

                        readonly property var playbackNodes: {
                            const list = Pipewire.nodes.values;
                            return list.filter(n => n.isStream && n.audio &&
                                (n.type & PwNodeType.Audio) && (n.type & PwNodeType.Sink) && (n.type & PwNodeType.Stream));
                        }

                        // PwObjectTracker is a plain QtObject, not an Item —
                        // it has no `parent` property to fall back on, so
                        // the bare `parent.playbackNodes` this used to read
                        // threw "parent is not defined" the first time this
                        // popup actually rendered (caught wiring up the
                        // notifications IPC toggle — this section was never
                        // exercised live before). Reference the ColumnLayout
                        // by id instead.
                        PwObjectTracker {
                            objects: volumeSection.playbackNodes
                        }

                        Repeater {
                            model: volumeSection.playbackNodes
                            delegate: RowLayout {
                                required property var modelData
                                Layout.fillWidth: true
                                spacing: 8

                                Text {
                                    text: modelData.description || modelData.name
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(11)
                                    Layout.preferredWidth: 100
                                    elide: Text.ElideRight
                                }

                                Rectangle {
                                    Layout.fillWidth: true
                                    height: 5
                                    radius: 2.5
                                    color: Theme.surface2

                                    Rectangle {
                                        width: parent.width * Math.min(1, modelData.audio.volume)
                                        height: parent.height
                                        radius: 2.5
                                        color: Theme.blue
                                    }

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: mouse => {
                                            modelData.audio.volume = mouse.x / width;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // --- Notification list, grouped by app ---
                    SectionHeader { label: "Notifications"; visible: Notifications.trackedNotifications.values.length > 0 }

                    Flickable {
                        id: notifFlick
                        Layout.fillWidth: true
                        // Explicit cap, not fillHeight — the window's own
                        // implicitHeight is now derived from this column's
                        // content (see the content-fit PanelWindow sizing
                        // above), so fillHeight here would be a circular
                        // dependency (Flickable waiting on a window height
                        // that's itself waiting on this Flickable). 320px
                        // comfortably fits several groups before scrolling.
                        Layout.preferredHeight: Math.min(320, notifList.implicitHeight)
                        contentHeight: notifList.implicitHeight
                        clip: true

                        // Ephemeral, defaults to expanded — resets whenever
                        // the panel's own LazyLoader unloads/reloads this
                        // PanelWindow on close/reopen. No cross-file
                        // persistence needed unless that default ever feels
                        // wrong in practice.
                        property var expandedGroups: ({})
                        function isExpanded(appName) {
                            return notifFlick.expandedGroups[appName] !== false;
                        }
                        function toggleGroup(appName) {
                            const next = Object.assign({}, notifFlick.expandedGroups);
                            next[appName] = !notifFlick.isExpanded(appName);
                            notifFlick.expandedGroups = next;
                        }

                        readonly property var groupedNotifications: {
                            const list = Notifications.trackedNotifications.values;
                            const groups = {};
                            const order = [];
                            for (const n of list) {
                                const key = n.appName || "Unknown";
                                if (!groups[key]) { groups[key] = []; order.push(key); }
                                groups[key].push(n);
                            }
                            return order.map(key => ({ appName: key, items: groups[key] }));
                        }

                        ColumnLayout {
                            id: notifList
                            width: parent.width
                            spacing: 8

                            Text {
                                visible: Notifications.trackedNotifications.values.length === 0
                                text: "No Notifications"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(13)
                                Layout.alignment: Qt.AlignHCenter
                                Layout.topMargin: 30
                            }

                            Repeater {
                                model: notifFlick.groupedNotifications
                                delegate: ColumnLayout {
                                    id: groupDelegate
                                    required property var modelData
                                    Layout.fillWidth: true
                                    spacing: 4

                                    readonly property bool expanded: notifFlick.isExpanded(modelData.appName)

                                    // Group header: app icon, name, count, collapse chevron.
                                    RowLayout {
                                        Layout.fillWidth: true
                                        spacing: 6

                                        IconImage {
                                            readonly property string iconSource: modelData.items[0].appIcon !== "" ? Quickshell.iconPath(modelData.items[0].appIcon, true) : ""
                                            visible: iconSource !== ""
                                            source: iconSource
                                            implicitSize: 16
                                            asynchronous: true
                                        }

                                        Text {
                                            text: modelData.appName
                                            color: Theme.subtext1
                                            font.family: Config.fontFamily
                                            font.bold: true
                                            font.pixelSize: Config.px(11)
                                        }

                                        Text {
                                            text: modelData.items.length
                                            color: Theme.overlay0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(10)
                                        }

                                        Item { Layout.fillWidth: true }

                                        Text {
                                            text: groupDelegate.expanded ? "\u{F0140}" : "\u{F0142}"
                                            color: Theme.overlay0
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(12)

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: notifFlick.toggleGroup(groupDelegate.modelData.appName)
                                            }
                                        }
                                    }

                                    ColumnLayout {
                                        Layout.fillWidth: true
                                        Layout.leftMargin: 4
                                        spacing: 6
                                        visible: groupDelegate.expanded

                                        Repeater {
                                            model: modelData.items
                                            delegate: Rectangle {
                                                required property var modelData
                                                Layout.fillWidth: true
                                                implicitHeight: itemContent.implicitHeight + 16
                                                radius: 8
                                                color: Theme.surface1

                                                ColumnLayout {
                                                    id: itemContent
                                                    anchors.fill: parent
                                                    anchors.margins: 8
                                                    spacing: 4

                                                    RowLayout {
                                                        Text {
                                                            text: modelData.summary
                                                            color: Theme.text
                                                            font.family: Config.fontFamily
                                                            font.bold: true
                                                            font.pixelSize: Config.px(12)
                                                            Layout.fillWidth: true
                                                            elide: Text.ElideRight
                                                        }
                                                        Text {
                                                            // Reading TimeFormat.tick (even unused) makes this binding
                                                            // depend on it, so the label re-evaluates every minute
                                                            // instead of computing once and going stale.
                                                            text: { TimeFormat.tick; return TimeFormat.relative(Notifications.receivedAt(modelData.id)); }
                                                            color: Theme.overlay0
                                                            font.family: Config.fontFamily
                                                            font.pixelSize: Config.px(10)
                                                        }
                                                        Text {
                                                            text: "\u{F0156}"
                                                            color: Theme.overlay0
                                                            font.family: Config.fontFamily
                                                            font.pixelSize: Config.px(12)

                                                            MouseArea {
                                                                anchors.fill: parent
                                                                onClicked: modelData.dismiss()
                                                            }
                                                        }
                                                    }

                                                    Text {
                                                        visible: modelData.body !== ""
                                                        text: modelData.body
                                                        color: Theme.subtext0
                                                        font.family: Config.fontFamily
                                                        font.pixelSize: Config.px(11)
                                                        wrapMode: Text.WordWrap
                                                        Layout.fillWidth: true
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
            }
        }
    }
}
