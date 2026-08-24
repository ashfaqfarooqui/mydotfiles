import QtQuick
import QtQuick.Effects
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/ws + hyprland/workspaces modules: a leading
// icon, then buttons 1-10 (persistent, like waybar's persistent-workspaces),
// highlighting the focused one. Scroll to switch, same as the original
// on-scroll-up/down dispatchers.
//
// This machine's Hyprland build has a Lua-native dispatch IPC (hl.dispatch /
// hl.dsp.*, see hypr/.config/hypr/conf/keybindings.lua) instead of vanilla
// Hyprland's classic "dispatch <dispatcher> <args>" text protocol — a plain
// "workspace N" string gets rejected with a Lua parse error. Dispatch calls
// here must be Lua dispatcher-object expressions instead, confirmed live via
// `hyprctl dispatch 'hl.dsp.focus({workspace = N, on_current_monitor = true})'`.
//
// One instance of this component lives on every monitor's Bar, so "focused"
// can't mean the single global Hyprland.focusedWorkspace — that would make
// every monitor's row highlight the same number. Instead each instance
// resolves its own HyprlandMonitor (via the `screen` prop threaded down from
// Bar.qml) and reads that monitor's own activeWorkspace. A further
// `isFocusedMonitorActive` cue marks the one workspace, on the one monitor,
// that also currently holds keyboard focus.
//
// Root is a plain Item replicating GroupPill's own pill background (radius
// 8, Theme.surface1 @ 0.35) rather than instantiating GroupPill directly —
// GroupPill's inner RowLayout would fight the sliding-indicator Rectangle
// below for control of each delegate's x position (RowLayout re-positions
// every pass), whereas a plain Row leaves each delegate's x stable and
// readable via Repeater.itemAt(i).x, which the slider needs every frame it
// animates.
Item {
    id: root
    property var screen
    readonly property var monitor: screen ? Hypr.monitorFor(screen) : null
    readonly property bool monitorHasKeyboardFocus: monitor?.focused ?? false

    readonly property var icons: ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

    readonly property int padding: 6
    implicitWidth: pillArea.width + padding * 2
    implicitHeight: pillArea.height + padding
    width: implicitWidth
    height: implicitHeight

    // hl.dsp.focus's `monitor` field silently drops a paired `workspace`
    // field (confirmed live: `hl.dsp.focus({workspace = 3, monitor = "X"})`
    // moves focus to monitor X but leaves its active workspace unchanged) —
    // so switching a workspace on a monitor that isn't currently focused
    // takes two calls: focus that monitor, then switch its workspace via
    // on_current_monitor. This does mean clicking/scrolling a non-focused
    // monitor's row also moves keyboard focus there, as a side effect.
    function focusWorkspace(target) {
        if (!root.monitorHasKeyboardFocus && root.screen) {
            Hypr.dispatch("hl.dsp.focus({monitor = \"" + root.screen.name + "\"})");
        }
        Hypr.dispatch("hl.dsp.focus({workspace = " + target + ", on_current_monitor = true})");
    }

    Rectangle {
        anchors.fill: parent
        radius: 8
        color: Theme.surface1
        opacity: 0.35
    }

    Item {
        id: pillArea
        anchors.centerIn: parent
        width: content.implicitWidth
        height: content.implicitHeight

        readonly property int focusedIndex: (root.monitor?.activeWorkspace?.id ?? 1) - 1
        readonly property var focusedItem: wsRepeater.count > 0 ? wsRepeater.itemAt(focusedIndex) : null

        // Sliding focus indicator — a pure "where is focus" marker layered
        // behind the delegates. Deliberately a single neutral fill, not
        // re-deriving the existing focused/occupied/empty color logic
        // (that stays on the number text + underline, unchanged below).
        // Drop shadow mirrors waybar's old #workspaces button.active box-
        // shadow (style.css: 0 0 6px 1px rgba(0,0,0,0.4)) — the one visual
        // cue the QML port was missing that made the active box read as
        // flat instead of "lifted".
        Rectangle {
            id: slider
            visible: pillArea.focusedItem !== null
            z: -1
            radius: 5
            color: Theme.surface2
            opacity: 0.75
            // Wraps just the number glyph's own box (plus a hair of
            // breathing room), not the full delegate Column — including
            // the underline's spacing/height made the pill look tall and
            // blobby relative to how narrow the digits are.
            height: (pillArea.focusedItem ? pillArea.focusedItem.numberHeight : 0) + 2
            // Delegates are top-aligned within the Row (matching pillArea's
            // own y=0), and the number Text sits at the top of its Column —
            // so wrapping it is a small negative offset, not vertical
            // centering against the taller full-column height.
            y: -1
            x: pillArea.focusedItem ? pillArea.focusedItem.x : 0
            width: pillArea.focusedItem ? pillArea.focusedItem.width : 0

            Behavior on x { NumberAnimation { duration: 160; easing.type: Easing.OutCubic } }
            Behavior on width { NumberAnimation { duration: 160; easing.type: Easing.OutCubic } }

            layer.enabled: true
            layer.effect: MultiEffect {
                shadowEnabled: true
                shadowColor: Qt.rgba(Theme.crust.r, Theme.crust.g, Theme.crust.b, 0.55)
                shadowBlur: 0.5
                shadowVerticalOffset: 1
                shadowHorizontalOffset: 0
                shadowOpacity: 0.6
            }
        }

        Row {
            id: content
            spacing: 2

            Text {
                text: ""
                color: Theme.overlay0
                font.family: Config.fontFamily
                font.pixelSize: Settings.fontSize
                font.weight: Config.fontWeight
                anchors.verticalCenter: parent.verticalCenter
                rightPadding: 4
            }

            Repeater {
                id: wsRepeater
                model: 10
                delegate: Column {
                    required property int index
                    readonly property int wsId: index + 1
                    readonly property bool isFocused: root.monitor?.activeWorkspace?.id === wsId
                    // Mirrors waybar's button.occupied class — a workspace with at
                    // least one window on it, regardless of focus.
                    readonly property bool isOccupied: (Hypr.workspaces.values.find(w => w.id === wsId)?.toplevels.values.length ?? 0) > 0
                    // True only for the single workspace, on the single monitor,
                    // that is both that monitor's active workspace and on the
                    // monitor currently holding keyboard focus.
                    readonly property bool isFocusedMonitorActive: isFocused && root.monitorHasKeyboardFocus
                    // Exposed so the sliding pill can wrap just the number
                    // glyph's own box, not this Column's full height
                    // (number + spacing + underline) — that made the pill
                    // read as too tall/blobby relative to the digits.
                    readonly property alias numberHeight: numberText.height

                    spacing: 2

                    Text {
                        id: numberText
                        text: root.icons[index]
                        color: isFocused ? Theme.blue : (isOccupied ? Theme.mauve : Theme.overlay0)
                        font.family: Config.fontFamily
                        font.pixelSize: Settings.fontSize
                        font.weight: Config.fontWeight
                        font.bold: isFocused
                        leftPadding: 7
                        rightPadding: 7
                        topPadding: 2
                        bottomPadding: 2
                        horizontalAlignment: Text.AlignHCenter

                        MouseArea {
                            anchors.fill: parent
                            onClicked: root.focusWorkspace(String(wsId))
                        }
                    }

                    Rectangle {
                        visible: isOccupied
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: parent.width - 8
                        height: isFocusedMonitorActive ? 3 : 2
                        radius: 1
                        color: isFocusedMonitorActive ? Theme.lavender : (isFocused ? Theme.blue : Theme.mauve)
                    }
                }
            }
        }
    }

    WheelHandler {
        target: root
        onWheel: event => {
            const rel = event.angleDelta.y < 0 ? "+1" : "-1";
            root.focusWorkspace("\"" + rel + "\"");
        }
    }
}
