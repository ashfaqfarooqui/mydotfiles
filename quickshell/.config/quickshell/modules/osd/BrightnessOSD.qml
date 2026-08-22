import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.theme
import qs.config
import qs.services

// New: transient brightness OSD, same rationale as VolumeOSD.qml.
Scope {
    id: root
    property bool shouldShow: false
    property int _lastPercent: Brightness.percent

    Connections {
        target: Brightness
        function onPercentChanged() {
            if (Brightness.percent !== root._lastPercent) {
                root._lastPercent = Brightness.percent;
                root.shouldShow = true;
                hideTimer.restart();
            }
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
                        text: "󰃟"
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
                            width: parent.width * (Brightness.percent / 100)
                            height: parent.height
                            radius: 3
                            color: Theme.yellow
                        }
                    }

                    Text {
                        text: Brightness.percent + "%"
                        color: Theme.text
                        font.family: Config.fontFamily
                        font.pixelSize: 14
                    }
                }
            }
        }
    }
}
