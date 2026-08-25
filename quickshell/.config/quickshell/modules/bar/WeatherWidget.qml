import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/weather module. Click opens WeatherPanel.qml
// (current conditions + 3-day forecast) — same icon+panel convention as
// NetworkWidget.qml/BluetoothWidget.qml, instead of the old bare-text pill.
Item {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    implicitWidth: row.implicitWidth
    implicitHeight: row.implicitHeight

    Row {
        id: row
        anchors.centerIn: parent
        spacing: 4

        Text {
            anchors.verticalCenter: parent.verticalCenter
            text: Weather.loaded ? Weather.iconForCode(Weather.weatherCode, !Weather.isDay) : "\u{F0599}"
            color: Theme.text
            font.family: Config.fontFamily
            font.pixelSize: Settings.fontSize
            font.weight: Config.fontWeight
        }

        Text {
            anchors.verticalCenter: parent.verticalCenter
            text: Weather.text + "°"
            color: Theme.text
            font.family: Config.fontFamily
            font.pixelSize: Settings.fontSize
            font.weight: Config.fontWeight
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: Weather.togglePanel(Screen.name)
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(Weather.tooltip, point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
