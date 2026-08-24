import QtQuick
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
Row {
    id: root
    spacing: 2

    readonly property var icons: ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

    Text {
        text: "\uF303"
        color: Theme.overlay0
        font.family: Config.fontFamily
        font.pixelSize: Settings.fontSize
        font.weight: Config.fontWeight
        anchors.verticalCenter: parent.verticalCenter
        rightPadding: 4
    }

    Repeater {
        model: 10
        delegate: Column {
            required property int index
            readonly property int wsId: index + 1
            readonly property bool isFocused: Hypr.focusedWorkspace?.id === wsId
            // Mirrors waybar's button.occupied class — a workspace with at
            // least one window on it, regardless of focus.
            readonly property bool isOccupied: (Hypr.workspaces.values.find(w => w.id === wsId)?.toplevels.values.length ?? 0) > 0

            spacing: 1

            Text {
                text: root.icons[index]
                color: isFocused ? Theme.blue : (isOccupied ? Theme.mauve : Theme.overlay0)
                font.family: Config.fontFamily
                font.pixelSize: Settings.fontSize
                font.weight: Config.fontWeight
                font.bold: isFocused
                leftPadding: 4
                rightPadding: 4
                horizontalAlignment: Text.AlignHCenter

                MouseArea {
                    anchors.fill: parent
                    onClicked: Hypr.dispatch("hl.dsp.focus({workspace = " + wsId + ", on_current_monitor = true})")
                }
            }

            Rectangle {
                visible: isOccupied
                anchors.horizontalCenter: parent.horizontalCenter
                width: parent.width - 8
                height: 2
                radius: 1
                color: isFocused ? Theme.blue : Theme.mauve
            }
        }
    }

    WheelHandler {
        target: root
        onWheel: event => {
            const rel = event.angleDelta.y < 0 ? "+1" : "-1";
            Hypr.dispatch("hl.dsp.focus({workspace = \"" + rel + "\", on_current_monitor = true})");
        }
    }
}
