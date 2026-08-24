import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "network" module.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    readonly property var wifiIcons: ["󰤮", "󰤯", "󰤟", "󰤢", "󰤥", "󰤨"]

    text: {
        if (Network.kind === "ethernet") return "󰈀";
        if (Network.kind === "wifi") {
            const idx = Math.min(wifiIcons.length - 1, Math.floor(Network.signalStrength / 20));
            return wifiIcons[idx] + " " + Network.signalStrength + "%";
        }
        return "󰌙";
    }
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Network.togglePanel(Screen.name);
            else Quickshell.execDetached(["ghostty", "-e", "bash", "-c", "nmtui"]);
        }
    }

    HoverHandler {
        onHoveredChanged: {
            if (hovered) {
                Network.refreshIpInfo();
                TooltipBus.show(
                    (Network.kind === "wifi" ? (Network.ssid + " (" + Network.signalStrength + "%)") :
                    Network.kind === "ethernet" ? "Ethernet connected" : "Disconnected") +
                    (Network.localIp !== "" ? "\n" + Network.localIp : "") +
                    (Network.vpnActive ? "\nVPN active" : "")
                , point.scenePosition.x, root.screenName);
            } else {
                TooltipBus.hide();
            }
        }
    }
}
