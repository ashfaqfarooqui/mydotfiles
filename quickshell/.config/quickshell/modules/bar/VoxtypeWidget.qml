import QtQuick
import QtQuick.Window
import Quickshell
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "custom/voxtype" module (push-to-talk dictation
// status). Left-click toggles recording the same way the Hyprland
// keybinding does (`voxtype record toggle`, see conf/keybindings.lua);
// right-click restarts the daemon, matching voxtype-module.jsonc's
// on-click. Color states mirror style.css's #custom-voxtype.* rules:
// idle -> normal text, transcribing -> warning, recording -> critical
// (pulsing there; a flat color here, no CSS animation equivalent),
// stopped -> dim.
Text {
    id: root
    // Screen must be captured here (a real Item), not read inside
    // HoverHandler below — see IdleToggle.qml for why.
    readonly property string screenName: Screen.name

    // A waveform glyph (mdi-waveform, U+F147D), deliberately not the same
    // microphone glyph PrivacyIndicator.qml uses for micInUse — the two
    // used to be visually identical, making it impossible to tell "voxtype
    // is listening" apart from "something else has the mic open" at a
    // glance.
    text: "󱑽"
    color: {
        switch (Voxtype.state) {
        case "recording": return Theme.red;
        case "transcribing": return Theme.yellow;
        case "stopped": return Theme.overlay0;
        default: return Theme.text;
        }
    }
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Quickshell.execDetached(["voxtype", "record", "toggle"]);
            else Quickshell.execDetached(["systemctl", "--user", "restart", "voxtype"]);
        }
    }

    HoverHandler {
        onHoveredChanged: hovered ? TooltipBus.show(Voxtype.tooltip, point.scenePosition.x, root.screenName) : TooltipBus.hide()
    }
}
