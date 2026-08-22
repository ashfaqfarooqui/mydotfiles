import QtQuick
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "network" module.
Text {
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
    font.pixelSize: Config.fontSize

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Quickshell.execDetached(["nm-applet"]);
            else Quickshell.execDetached(["ghostty", "-e", "bash", "-c", "nmtui"]);
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(
            Network.kind === "wifi" ? (Network.ssid + " (" + Network.signalStrength + "%)") :
            Network.kind === "ethernet" ? "Ethernet connected" : "Disconnected"
        ) : TooltipBus.hide()
    }
}
