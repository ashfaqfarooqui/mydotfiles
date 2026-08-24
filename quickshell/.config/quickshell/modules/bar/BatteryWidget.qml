import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "battery" module.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    readonly property int pct: Math.round(Battery.percent)

    text: Battery.iconFor(pct, Battery.charging) + " " + pct + "%"
    color: pct <= 15 && !Battery.charging ? Theme.red : Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        onClicked: Battery.togglePanel(Screen.name)
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            (Battery.charging ? "Charging: " + pct + "%" : "Discharging: " + pct + "%") +
            (Battery.charging && Battery.timeToFullFormatted !== "" ? "\nTime to full: " + Battery.timeToFullFormatted :
                !Battery.charging && Battery.timeToEmptyFormatted !== "" ? "\nTime remaining: " + Battery.timeToEmptyFormatted : "") +
            (Battery.powerDrawWatts > 0 ? "\n" + Battery.powerDrawWatts.toFixed(1) + " W" : "")
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
