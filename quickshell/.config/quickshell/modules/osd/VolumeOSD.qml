import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.theme
import qs.config
import qs.services

// New: a transient volume OSD popup. Waybar had no equivalent (volume was
// only visible in the always-on bar module); this mirrors the pattern from
// quickshell's own volume-osd example (git.outfoxxed.me/quickshell/
// quickshell-examples), shown on the focused monitor only.
Scope {
    id: root
    property bool shouldShow: false

    onVolumeChangedTrigger: {
        shouldShow = true;
        hideTimer.restart();
    }

    // Audio.volumePercent starts at a default before Pipewire finishes its
    // initial sync; without this guard, that first real-value update looks
    // like a user-triggered volume change and pops the OSD on launch.
    property bool _ready: false
    property int _lastPercent: Audio.volumePercent
    signal volumeChangedTrigger()
    Connections {
        target: Audio
        function onVolumePercentChanged() {
            if (!root._ready) {
                root._ready = true;
                root._lastPercent = Audio.volumePercent;
                return;
            }
            if (Audio.volumePercent !== root._lastPercent) {
                root._lastPercent = Audio.volumePercent;
                root.volumeChangedTrigger();
            }
        }
        function onMutedChanged() {
            if (!root._ready) return;
            root.volumeChangedTrigger();
        }
    }

    Timer {
        id: hideTimer
        interval: 1500
        onTriggered: root.shouldShow = false
    }

    LazyLoader {
        active: root.shouldShow

        PanelWindow {
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors.bottom: true
            margins.bottom: Math.round(screen.height * 0.12)
            implicitWidth: Config.px(320)
            implicitHeight: Config.px(56)
            color: "transparent"
            exclusiveZone: 0
            mask: Region {}

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0

                RowLayout {
                    anchors.fill: parent
                    anchors.margins: 12
                    spacing: 10

                    Text {
                        text: Audio.muted ? "" : ""
                        color: Theme.text
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(18)
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        height: 6
                        radius: 3
                        color: Theme.surface2

                        Rectangle {
                            width: parent.width * Math.min(1, Audio.volume)
                            height: parent.height
                            radius: 3
                            color: Audio.muted ? Theme.overlay0 : Theme.blue
                        }
                    }

                    Text {
                        text: Audio.volumePercent + "%"
                        color: Theme.text
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(14)
                    }
                }
            }
        }
    }
}
