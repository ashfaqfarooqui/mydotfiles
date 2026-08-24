import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's hyprland/window module (+ rewrite table).
// WindowRewriteRules is a neighboring file in this same directory, so it's
// implicitly available without an explicit import.
Text {
    readonly property var resolved: WindowRewriteRules.resolve(Hypr.activeToplevel?.title ?? "")

    text: (resolved.icon ? resolved.icon + " " : "") + resolved.label
    color: Theme.text
    font.family: Config.fontFamily
    font.pixelSize: Settings.fontSize
    font.weight: Config.fontWeight
    elide: Text.ElideRight
    width: Math.min(implicitWidth, 320)
}
