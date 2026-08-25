pragma Singleton
import QtQuick
import Quickshell
import qs.services
import qs.theme

// Static layout knobs, mirrors the two waybar configs (full bar on
// external outputs, reduced bar on the laptop panel eDP-1).
Singleton {
    readonly property int barHeight: 38
    readonly property string reducedOutput: "eDP-1"
    // Single source of truth is theme/font.json -> theme/quickshell.tera ->
    // theme.json -> Theme.fontFamily, same live-reloaded pipeline the
    // colors already use, so a font change doesn't need a shell restart.
    readonly property string fontFamily: Theme.fontFamily
    // Bar-row text weight is a fixed style choice, not something exposed to
    // the Display panel's text-size slider.
    readonly property int fontWeight: Font.DemiBold
    // privacy indicator's ignore-list, mirrors waybar's privacy.ignore config
    readonly property var privacyIgnore: ["cava", "obs"]

    // Settings.fontSize (the Display panel's "TEXT SIZE" slider) was
    // originally bar-only, on Omarchy's own stop list with 12 as the
    // default/neutral value — every font.pixelSize literal elsewhere in the
    // shell (launcher, OSDs, lock screen, notification popup, polkit
    // dialog) was hand-picked relative to that same 12px baseline. uiScale
    // turns the slider into a shell-wide multiplier instead of a bar-only
    // one, and px() is the one place that ratio is computed — every other
    // file calls Config.px(<original literal>) instead of recomputing the
    // ratio itself, mirroring how color flows through Theme.qml.
    readonly property real uiScale: Settings.fontSize / 12
    function px(size) {
        return Math.round(size * uiScale);
    }
}
