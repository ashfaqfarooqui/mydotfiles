import QtQuick
import QtQuick.Window
import qs.theme
import qs.config
import qs.services

// Opens AgentsPanel.qml (local Claude Code token usage). No numeric badge —
// there's no live quota % available locally to show at a glance (see
// services/AgentsUsage.qml for why).
Text {
    id: root
    readonly property string screenName: Screen.name

    text: "\u{F0AE2}" // nf-md-star_four_points
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        onClicked: AgentsUsage.togglePanel(Screen.name)
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show("Claude Code usage", point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
