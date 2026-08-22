import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's clock#date module, including its hover calendar
// (waybar's tooltip-format: "{calendar}", a year-grid view). This ports it
// as a single-month grid instead of the full year — the year grid is a lot
// of extra layout for a feature that's only ever glanced at, and a month
// view still answers "what's today's date / what day does X fall on."
Item {
    id: root
    implicitWidth: label.implicitWidth
    implicitHeight: label.implicitHeight

    property date now: new Date()
    property real hoverX: 0
    // Month being displayed in the popup; independent of `now` so
    // prev/next navigation doesn't affect the bar label itself.
    property int viewYear: now.getFullYear()
    property int viewMonth: now.getMonth()

    Timer {
        interval: 60000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.now = new Date()
    }

    Text {
        id: label
        text: "󰨳 " + Qt.formatDateTime(root.now, "ddd MM-dd")
        color: Theme.text
        font.family: Config.fontFamily
        font.pixelSize: Config.fontSize
    }

    HoverHandler {
        id: hover
        onHoveredChanged: {
            if (hovered) {
                root.hoverX = point.scenePosition.x;
                root.viewYear = root.now.getFullYear();
                root.viewMonth = root.now.getMonth();
            }
        }
    }

    LazyLoader {
        active: hover.hovered

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true; left: true }
            margins.top: Config.barHeight + 4
            margins.left: Math.max(4, Math.min(root.hoverX - implicitWidth / 2, screen.width - implicitWidth - 4))
            implicitWidth: 240
            implicitHeight: 260
            color: "transparent"
            exclusiveZone: 0
            mask: Region {}

            readonly property date monthStart: new Date(root.viewYear, root.viewMonth, 1)
            // getDay(): 0=Sun..6=Sat; shift to a Monday-first grid.
            readonly property int leadingBlanks: (monthStart.getDay() + 6) % 7
            readonly property int daysInMonth: new Date(root.viewYear, root.viewMonth + 1, 0).getDate()
            readonly property var cells: {
                const arr = [];
                for (let i = 0; i < leadingBlanks; i++) arr.push(null);
                for (let d = 1; d <= daysInMonth; d++) arr.push(d);
                return arr;
            }

            Rectangle {
                anchors.fill: parent
                radius: 10
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 12
                    spacing: 8

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: Qt.formatDate(win.monthStart, "MMMM yyyy")
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: 13
                            Layout.fillWidth: true
                        }
                    }

                    GridLayout {
                        Layout.fillWidth: true
                        columns: 7
                        rowSpacing: 4
                        columnSpacing: 4

                        Repeater {
                            model: ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"]
                            delegate: Text {
                                required property string modelData
                                text: modelData
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: 10
                                Layout.preferredWidth: 26
                                horizontalAlignment: Text.AlignHCenter
                            }
                        }

                        Repeater {
                            model: win.cells
                            delegate: Item {
                                required property var modelData
                                Layout.preferredWidth: 26
                                Layout.preferredHeight: 22

                                readonly property bool isToday: modelData !== null &&
                                    root.viewYear === root.now.getFullYear() &&
                                    root.viewMonth === root.now.getMonth() &&
                                    modelData === root.now.getDate()

                                Rectangle {
                                    visible: isToday
                                    anchors.centerIn: parent
                                    width: 20
                                    height: 20
                                    radius: 10
                                    color: Theme.blue
                                }

                                Text {
                                    visible: modelData !== null
                                    anchors.centerIn: parent
                                    text: modelData ?? ""
                                    color: isToday ? Theme.base : Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: 11
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
