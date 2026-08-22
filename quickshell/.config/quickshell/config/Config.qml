pragma Singleton
import Quickshell

// Static layout knobs, mirrors the two waybar configs (full bar on
// external outputs, reduced bar on the laptop panel eDP-1).
Singleton {
    readonly property int barHeight: 38
    readonly property string reducedOutput: "eDP-1"
}
