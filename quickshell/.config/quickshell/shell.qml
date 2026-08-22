import Quickshell
import qs.modules.bar
import qs.modules.osd

// Phase 1: full-parity bar (one per monitor, full/reduced split by output
// name) + volume/brightness OSD popups.
ShellRoot {
    Variants {
        model: Quickshell.screens
        Bar {}
    }

    VolumeOSD {}
    BrightnessOSD {}
}
