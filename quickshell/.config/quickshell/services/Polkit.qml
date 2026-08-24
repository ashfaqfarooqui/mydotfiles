pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Services.Polkit

// Replaces hyprpolkitagent. Uses Quickshell.Services.Polkit.PolkitAgent
// (confirmed present at /usr/lib/qt6/qml/Quickshell/Services/Polkit on this
// system's quickshell 0.3.1) — only one polkit agent may be registered
// system-wide, so `hyprpolkitagent` must stop being autostarted (see
// hypr/.config/hypr/conf/autostart.lua) or the two will race for
// registration. modules/polkit/PolkitDialog.qml (instantiated from
// shell.qml, same as every other top-level popup) is the actual UI.
Singleton {
    id: root

    readonly property var flow: agent.flow
    readonly property bool dialogVisible: agent.isActive

    PolkitAgent {
        id: agent
        path: "/org/quickshell/PolkitAgent"

        onIsRegisteredChanged: {
            if (!isRegistered) console.warn("quickshell polkit agent failed to register — another agent may already be running");
        }
    }
}
