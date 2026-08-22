import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's two-config split: a full bar on external outputs, a
// reduced bar on the laptop panel (Config.reducedOutput, "eDP-1").
PanelWindow {
    id: root
    required property var modelData
    screen: modelData

    readonly property bool reduced: screen.name === Config.reducedOutput

    anchors {
        top: true
        left: true
        right: true
    }
    implicitHeight: Config.barHeight
    // Matches waybar's window#waybar>box background: rgba(17,17,27,0.55) —
    // a translucent surface so Hyprland's layer blur (the "shell:*"/"bar"
    // layer_rules already matching this namespace by substring) has
    // something to actually blend, instead of a fully opaque fill.
    color: Qt.rgba(Theme.base.r, Theme.base.g, Theme.base.b, 0.55)

    Item {
        anchors.fill: parent
        anchors.leftMargin: 8
        anchors.rightMargin: 8

        Row {
            id: leftRow
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            spacing: 10

            Workspaces {}
            Submap {}
            ActiveWindow {}
        }

        Row {
            anchors.centerIn: parent
            spacing: 10

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                visible: !root.reduced
                WeatherWidget {}
                TemperatureWidget {}
                MemoryWidget {}
                CpuWidget {}
                DiskWidget {}
            }
            Separator { visible: !root.reduced }

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                IdleToggle {}
                ClockWidget {}
                DateWidget {}
            }
            Separator {}

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                NetworkWidget {}
                NetSpeedWidget {}
                BluetoothWidget {}
                TrayWidget {}
            }
        }

        Row {
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            spacing: 10

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                PrivacyIndicator {}
                LanguageWidget {}
                MprisWidget { visible: !root.reduced }
            }
            Separator {}

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                VolumeWidget {}
                BacklightWidget { visible: !root.reduced }
            }
            Separator {}

            BatteryWidget {}
            Separator {}
            PowerButton {}
            NotificationBadge {}
        }
    }

    // Shared tooltip overlay, see services/TooltipBus.qml. Shows on both
    // monitors' bars at once (not tracking exact screen identity through
    // every widget) — an accepted minor imperfection with only two monitors.
    LazyLoader {
        active: TooltipBus.text !== ""

        PanelWindow {
            screen: root.screen
            anchors.top: true
            margins.top: Config.barHeight + 4
            implicitWidth: tooltipText.implicitWidth + 16
            implicitHeight: tooltipText.implicitHeight + 10
            color: "transparent"
            exclusiveZone: 0
            mask: Region {}

            Rectangle {
                anchors.fill: parent
                radius: 6
                color: Theme.surface0

                Text {
                    id: tooltipText
                    anchors.centerIn: parent
                    text: TooltipBus.text
                    color: Theme.text
                    font.family: Config.fontFamily
                    font.pixelSize: 12
                }
            }
        }
    }
}
