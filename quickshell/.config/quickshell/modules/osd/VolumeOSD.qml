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

    property int _lastPercent: Audio.volumePercent
    signal volumeChangedTrigger()
    Connections {
        target: Audio
        function onVolumePercentChanged() {
            if (Audio.volumePercent !== root._lastPercent) {
                root._lastPercent = Audio.volumePercent;
                root.volumeChangedTrigger();
            }
        }
        function onMutedChanged() {
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
            implicitWidth: 320
            implicitHeight: 56
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
                        font.pixelSize: 18
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
                        font.pixelSize: 14
                    }
                }
            }
        }
    }
}
