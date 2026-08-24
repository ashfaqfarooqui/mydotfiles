import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    text: "󰨣 " + SystemStats.diskPercent + "%"
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        onClicked: SystemStats.togglePanel(Screen.name)
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            "Used: " + SystemStats.diskUsed + " / " + SystemStats.diskTotal + " (" + SystemStats.diskPercent + "%)\n" +
            "Free: " + SystemStats.diskFree
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
