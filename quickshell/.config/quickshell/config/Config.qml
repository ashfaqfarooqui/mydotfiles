pragma Singleton
import QtQuick
import Quickshell

// Static layout knobs, mirrors the two waybar configs (full bar on
// external outputs, reduced bar on the laptop panel eDP-1).
Singleton {
    readonly property int barHeight: 38
    readonly property string reducedOutput: "eDP-1"
    readonly property string fontFamily: "JetBrainsMono Nerd Font"
    // Bar-row text weight — bar font *size* lives in Settings.qml instead
    // (user-adjustable via the Display panel's text-size slider), but the
    // weight is a fixed style choice, not something exposed to the picker.
    readonly property int fontWeight: Font.DemiBold
    // privacy indicator's ignore-list, mirrors waybar's privacy.ignore config
    readonly property var privacyIgnore: ["cava", "obs"]
}
