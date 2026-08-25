import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Weather detail popup, opened by left-clicking WeatherWidget.qml. Scoped-
// down port of basecamp/omarchy's shell/plugins/panels/weather: current
// conditions + a 3-day forecast strip, minus its location search/picker
// (this repo's location is fixed — see Weather.qml).
Scope {
    id: root

    required property string screenName
    readonly property bool isActive: Weather.panelVisible && Weather.panelScreenName === screenName

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true }
            margins {
                top: Config.barHeight + 4
                left: Math.max(4, Math.round((screen.width - implicitWidth) / 2))
            }
            implicitWidth: Config.px(340)
            implicitHeight: content.implicitHeight + 32
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: Weather.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Weather.hidePanel()

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 18
                    spacing: 16

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 14

                        Text {
                            text: Weather.loaded ? Weather.iconForCode(Weather.weatherCode, !Weather.isDay) : "\u{F0599}"
                            color: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(34)
                        }

                        ColumnLayout {
                            spacing: 0
                            Layout.fillWidth: true

                            Text {
                                text: (Weather.loaded ? Math.round(Weather.currentTemp) : "…") + "°"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(22)
                            }
                            Text {
                                text: Weather.locationName + (Weather.loaded ? " · " + Weather.descriptionForCode(Weather.weatherCode) : "")
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(11)
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                            }
                        }
                    }

                    Rectangle { Layout.fillWidth: true; height: 1; color: Theme.surface2 }

                    RowLayout {
                        Layout.fillWidth: true
                        Layout.topMargin: 2
                        Layout.bottomMargin: 2
                        visible: Weather.loaded

                        component Stat: ColumnLayout {
                            property string label
                            property string value
                            Layout.alignment: Qt.AlignHCenter
                            Layout.fillWidth: true
                            spacing: 4
                            Text { text: parent.value; color: Theme.text; font.family: Config.fontFamily; font.bold: true; font.pixelSize: Config.px(14); Layout.alignment: Qt.AlignHCenter }
                            Text { text: parent.label; color: Theme.overlay0; font.family: Config.fontFamily; font.pixelSize: Config.px(9); Layout.alignment: Qt.AlignHCenter }
                        }

                        Item { Layout.fillWidth: true }
                        Stat { label: "FEELS LIKE"; value: Math.round(Weather.feelsLike) + "°"; Layout.fillWidth: false }
                        Item { Layout.fillWidth: true }
                        Rectangle { Layout.preferredWidth: 1; Layout.preferredHeight: 26; color: Theme.surface2 }
                        Item { Layout.fillWidth: true }
                        Stat { label: "HUMIDITY"; value: Math.round(Weather.humidity) + "%"; Layout.fillWidth: false }
                        Item { Layout.fillWidth: true }
                        Rectangle { Layout.preferredWidth: 1; Layout.preferredHeight: 26; color: Theme.surface2 }
                        Item { Layout.fillWidth: true }
                        Stat { label: "WIND"; value: Math.round(Weather.windSpeed) + " km/h"; Layout.fillWidth: false }
                        Item { Layout.fillWidth: true }
                    }

                    Rectangle { Layout.fillWidth: true; height: 1; color: Theme.surface2 }

                    RowLayout {
                        Layout.fillWidth: true

                        component ForecastDay: ColumnLayout {
                            property var day
                            Layout.alignment: Qt.AlignHCenter
                            spacing: 6

                            Text {
                                text: Weather.dayLabel(parent.day.date)
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                                Layout.alignment: Qt.AlignHCenter
                            }
                            Text {
                                text: Weather.iconForCode(parent.day.weatherCode, false)
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(20)
                                Layout.alignment: Qt.AlignHCenter
                            }
                            Text {
                                text: Math.round(parent.day.maxTemp) + "°/" + Math.round(parent.day.minTemp) + "°"
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(11)
                                Layout.alignment: Qt.AlignHCenter
                            }
                        }

                        Item { Layout.fillWidth: true }
                        ForecastDay { visible: Weather.forecast.length > 0; day: Weather.forecast[0] ?? ({}) }
                        Item { Layout.fillWidth: true }
                        ForecastDay { visible: Weather.forecast.length > 1; day: Weather.forecast[1] ?? ({}) }
                        Item { Layout.fillWidth: true }
                        ForecastDay { visible: Weather.forecast.length > 2; day: Weather.forecast[2] ?? ({}) }
                        Item { Layout.fillWidth: true }
                    }
                }
            }
        }
    }
}
