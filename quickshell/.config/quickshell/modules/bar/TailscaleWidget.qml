import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces basecamp/omarchy's tailscale bar widget with this repo's own
// icon+panel convention (see NetworkWidget.qml/BluetoothWidget.qml).
Item {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    implicitWidth: icon.implicitWidth
    implicitHeight: icon.implicitHeight

    TailscaleIcon {
        id: icon
        anchors.centerIn: parent
        iconSize: Config.px(14)
        tint: Tailscale.needsLogin ? Theme.yellow : (Tailscale.active ? Theme.blue : Theme.overlay0)
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Tailscale.togglePanel(Screen.name);
            else Tailscale.toggle();
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            "Tailscale: " + Tailscale.statusText + (Tailscale.selfIp !== "" ? "\n" + Tailscale.selfIp : "")
        , point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
