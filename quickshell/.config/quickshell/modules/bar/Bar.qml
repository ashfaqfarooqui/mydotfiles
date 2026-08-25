import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services
import qs.modules.network

// Same widget set on every monitor — no more waybar-style full/reduced
// split (Config.reducedOutput is now unused for layout purposes; kept only
// for BacklightWidget's real-backlight comment below).
PanelWindow {
    id: root
    required property var modelData
    screen: modelData

    anchors {
        top: true
        left: true
        right: true
    }
    implicitHeight: Config.barHeight
    // Bumped from waybar's original rgba(17,17,27,0.55) — 0.55 read as too
    // see-through against a busy wallpaper; still translucent enough for
    // Hyprland's layer blur (the "shell:*"/"bar" layer_rules) to blend.
    color: Qt.rgba(Theme.base.r, Theme.base.g, Theme.base.b, 0.78)

    Item {
        anchors.fill: parent
        anchors.leftMargin: 8
        anchors.rightMargin: 8

        Row {
            id: leftRow
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            spacing: 10

            // Workspaces/ActiveWindow are now differently-tall pill
            // widgets (their own padding, not just bare Text like Submap),
            // so a plain Row's default top-alignment left them visibly
            // misaligned against each other — same "Row only manages the
            // x-axis" pitfall GroupPill.qml's own header comment warns
            // about. Center each explicitly, matching how every GroupPill
            // in the center/right rows below already does this.
            Workspaces { anchors.verticalCenter: parent.verticalCenter; screen: root.screen }
            Submap { anchors.verticalCenter: parent.verticalCenter }
            ActiveWindow { anchors.verticalCenter: parent.verticalCenter }
        }

        Row {
            anchors.centerIn: parent
            spacing: 10

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                WeatherWidget {}
                TemperatureWidget {}
                MemoryWidget {}
                CpuWidget {}
                DiskWidget {}
            }
            Separator {}

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                IdleToggle {}
                ClockWidget {}
                DateWidget {}
            }
        }

        Row {
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            spacing: 10

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                PrivacyIndicator {}
                VoxtypeWidget {}
                LanguageWidget {}
                MprisWidget {}
                AgentsWidget {}
            }
            Separator {}

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                VolumeWidget {}
                // Shown on every monitor for a consistent bar even though
                // amdgpu_bl1 (confirmed via `brightnessctl -l`) is only the
                // laptop panel's own backlight — it still just controls that
                // one real backlight regardless of which screen it's clicked
                // from.
                BacklightWidget {}
            }
            Separator {}

            GroupPill {
                anchors.verticalCenter: parent.verticalCenter
                NetworkWidget {}
                BluetoothWidget {}
                TailscaleWidget {}
                TrayWidget {}
            }
            Separator {}

            RecordingIndicator {}
            BatteryWidget {}
            Separator {}
            PowerButton {}
            NotificationBadge {}
        }
    }

    NetworkPanel { screenName: root.screen.name }
    BluetoothPanel { screenName: root.screen.name }
    TailscalePanel { screenName: root.screen.name }
    WeatherPanel { screenName: root.screen.name }
    BatteryPanel { screenName: root.screen.name }
    DisplayPanel { screenName: root.screen.name }
    VolumePanel { screenName: root.screen.name }
    CalendarPanel { screenName: root.screen.name }
    VitalsPanel { screenName: root.screen.name }
    AgentsPanel { screenName: root.screen.name }

    // Shared tooltip overlay, see services/TooltipBus.qml. Gated on
    // TooltipBus.screenName (the hovered widget's own Screen.name) so only
    // the monitor actually being hovered shows a tooltip — previously both
    // monitors' Bar instances reacted to the same global TooltipBus.text
    // and each computed its own position from the same x against its own
    // screen.width, popping a second, wrongly-positioned tooltip on
    // whichever monitor wasn't hovered.
    LazyLoader {
        active: TooltipBus.text !== "" && TooltipBus.screenName === root.screen.name

        PanelWindow {
            screen: root.screen
            anchors { top: true; left: true }
            margins.top: Config.barHeight
            margins.left: Math.max(6, Math.min(TooltipBus.x - implicitWidth / 2, screen.width - implicitWidth - 6))
            implicitWidth: tooltipText.width + 24
            implicitHeight: tooltipText.implicitHeight + 18
            color: "transparent"
            exclusiveZone: 0
            mask: Region {}

            // Two-tone card: an outer accent border for contrast against the
            // wallpaper/blur behind it, and an inset inner border for depth
            // against the card's own fill — plain single-border/flat-fill
            // tooltips were hard to spot and read against a busy background.
            Rectangle {
                id: outer
                anchors.fill: parent
                radius: 10
                color: Theme.crust
                border.width: 1.5
                border.color: Qt.rgba(Theme.blue.r, Theme.blue.g, Theme.blue.b, 0.55)

                Rectangle {
                    anchors.fill: parent
                    anchors.margins: 2
                    radius: 8
                    color: "transparent"
                    border.width: 1
                    border.color: Qt.rgba(Theme.text.r, Theme.text.g, Theme.text.b, 0.08)
                }

                Text {
                    id: tooltipText
                    anchors.centerIn: parent
                    // wttrbar's weather tooltip embeds <b> pango markup, which
                    // makes Qt auto-detect RichText and switch to HTML
                    // semantics — where a literal "\n" is just whitespace,
                    // not a line break, collapsing the whole multi-line
                    // tooltip onto one line. Force RichText explicitly and
                    // translate "\n" to <br/> so every tooltip (plain or
                    // markup) renders as real separate lines; also bold the
                    // first line so multi-line tooltips read as a
                    // headline + detail instead of a flat block of text.
                    readonly property var lines: TooltipBus.text.split("\n")
                    textFormat: Text.RichText
                    text: lines.length > 1
                        ? "<b>" + lines[0] + "</b><br/><span style='color:" + Theme.subtext0 + "'>"
                            + lines.slice(1).join("<br/>") + "</span>"
                        : "<b>" + lines[0] + "</b>"
                    color: Theme.text
                    font.family: Config.fontFamily
                    font.pixelSize: Config.px(12)
                    lineHeight: 1.3
                    wrapMode: Text.WordWrap
                    width: Math.min(implicitWidth, 420)
                }
            }
        }
    }
}
