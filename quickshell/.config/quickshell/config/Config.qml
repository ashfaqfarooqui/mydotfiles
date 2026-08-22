pragma Singleton
import Quickshell

// Static layout knobs, mirrors the two waybar configs (full bar on
// external outputs, reduced bar on the laptop panel eDP-1).
Singleton {
    readonly property int barHeight: 38
    readonly property string reducedOutput: "eDP-1"
    readonly property string fontFamily: "JetBrainsMono Nerd Font"
    readonly property int fontSize: 13
    // privacy indicator's ignore-list, mirrors waybar's privacy.ignore config
    readonly property var privacyIgnore: ["cava", "obs"]
}
