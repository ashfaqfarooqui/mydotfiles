import QtQuick
import qs.theme
import qs.config
import qs.services

// Replaces waybar's hyprland/window module (+ rewrite table).
// WindowRewriteRules is a neighboring file in this same directory, so it's
// implicitly available without an explicit import. Wrapped in GroupPill so
// it reads as part of the same visual language as the bar's other pilled
// widget clusters instead of sitting as a bare Text sibling.
GroupPill {
    id: root
    padding: 6

    readonly property var resolved: WindowRewriteRules.resolve(Hypr.activeToplevel?.title ?? "")

    MarqueeText {
        text: (root.resolved.icon ? root.resolved.icon + " " : "") + root.resolved.label
        color: Theme.text
        maxWidth: 320
    }
}
