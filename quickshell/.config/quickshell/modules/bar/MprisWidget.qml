import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's "mpris" module. No need to import
// Quickshell.Services.Mpris directly here — qs.services.Mpris (this
// project's own wrapper singleton) already exposes everything needed, and
// importing the module's own "Mpris" singleton alongside it under the same
// unaliased name silently collided (see services/Mpris.qml for the fix).
Text {
    visible: Mpris.activePlayer !== null
    text: {
        const p = Mpris.activePlayer;
        if (!p) return "";
        return "♫ " + (p.trackTitle ?? "");
    }
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize
    elide: Text.ElideRight
    width: Math.min(implicitWidth, 250)

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton) Mpris.playPause();
            else if (mouse.button === Qt.RightButton) Mpris.next();
            else Mpris.previous();
        }
    }

    HoverHandler {
        onHoveredChanged: {
            const p = Mpris.activePlayer;
            hovered && p ? TooltipBus.show((p.identity ?? "") + "\n" + (p.trackTitle ?? "") + "\n" + (p.trackArtist ?? "")) : TooltipBus.hide();
        }
    }
}
