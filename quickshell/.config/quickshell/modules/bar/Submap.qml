import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's hyprland/submap module.
Text {
    visible: Hypr.submap !== ""
    text: "  " + Hypr.submap
    color: Theme.yellow
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize
    leftPadding: 8
    rightPadding: 8
}
