import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's custom/netspeed module. Original renders both lines
// via waybar's multi-line label support; joined with a space here since
// the bar is a single row.
Text {
    text: NetSpeed.text.replace("\n", "  ")
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Config.fontSize
}
