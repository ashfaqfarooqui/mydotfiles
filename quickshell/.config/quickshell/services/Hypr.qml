pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Hyprland

// Thin wrapper around the Quickshell.Hyprland singleton so bar/launcher
// modules import qs.services instead of Quickshell.Hyprland directly.
Singleton {
    readonly property var workspaces: Hyprland.workspaces
    readonly property var monitors: Hyprland.monitors
    readonly property var focusedWorkspace: Hyprland.focusedWorkspace
    readonly property var focusedMonitor: Hyprland.focusedMonitor
    readonly property var activeToplevel: Hyprland.activeToplevel
    readonly property var toplevels: Hyprland.toplevels

    // Hyprland has no "current submap" property; derive it from the event
    // socket, same source waybar's hyprland/submap module ultimately reads.
    property string submap: ""

    Connections {
        target: Hyprland
        function onRawEvent(event) {
            if (event.name === "submap") submap = event.data;
        }
    }

    function dispatch(request) {
        Hyprland.dispatch(request);
    }

    function monitorFor(screen) {
        return Hyprland.monitorFor(screen);
    }
}
