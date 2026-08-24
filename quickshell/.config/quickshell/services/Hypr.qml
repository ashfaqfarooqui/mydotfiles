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

            // HyprlandMonitor.activeWorkspace docs: "This is not updated
            // unless the monitor object is fetched again from Hyprland" —
            // and Hyprland.refreshMonitors()'s own doc warns "many actions
            // that will invalidate monitor/workspace state don't send
            // events." Confirmed live: moving a workspace to a
            // non-focused monitor left that monitor's Workspaces.qml
            // showing stale state until the user manually focused it.
            // Refreshing on every event (not just moveworkspace/
            // moveworkspacev2/focusedmon) is the only way to not miss a
            // state-changing action that Hyprland doesn't bother emitting
            // a dedicated event for.
            Hyprland.refreshMonitors();
            Hyprland.refreshWorkspaces();
        }
    }

    function dispatch(request) {
        Hyprland.dispatch(request);
    }

    function monitorFor(screen) {
        return Hyprland.monitorFor(screen);
    }

    // Exposed for callers (MonitorScale.qml) that change monitor state via
    // a plain `hyprctl eval`/Lua call rather than a dispatch — those don't
    // emit any event on the socket (confirmed live by listening on
    // .socket2.sock while triggering one), so the onRawEvent refresh above
    // never fires for them; the caller has to request its own refresh once
    // it knows the change actually landed.
    function refreshMonitors() {
        Hyprland.refreshMonitors();
    }
}
