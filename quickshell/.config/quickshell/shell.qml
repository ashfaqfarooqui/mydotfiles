import QtQuick
import Quickshell
import qs.config
import qs.theme
import qs.services

// Phase 0 scaffold: one placeholder PanelWindow per connected screen,
// wired to the Theme and Hypr singletons, to confirm the module structure
// and Hyprland IPC work before building out modules/bar/* in Phase 1.
ShellRoot {
    Variants {
        model: Quickshell.screens

        PanelWindow {
            id: bar
            property var modelData
            screen: modelData

            anchors {
                top: true
                left: true
                right: true
            }
            implicitHeight: Config.barHeight
            color: Theme.base

            Text {
                anchors.centerIn: parent
                color: Theme.text
                text: (Hypr.activeToplevel?.title ?? "Hyprland") + " — " + bar.screen.name
            }
        }
    }
}
