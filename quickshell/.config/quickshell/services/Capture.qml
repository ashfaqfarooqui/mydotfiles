pragma Singleton
import QtQuick
import Quickshell

// Backs CaptureMenu.qml (modules/capture/) and the two screenshot
// GridButtons in ControlCenter.qml. hyprshot without --clipboard-only
// already saves to $HYPRSHOT_DIR (falls back to XDG_PICTURES_DIR, then
// home) AND copies to the clipboard — confirmed via hyprshot's own docs —
// so screenshot() doesn't need a separate save step.
Singleton {
    id: root

    property bool menuVisible: false
    property bool recording: false

    function toggleMenu() {
        menuVisible = !menuVisible;
    }

    // mode is one of hyprshot's own -m values: "region"/"window"/"output".
    function screenshot(mode) {
        Quickshell.execDetached(["hyprshot", "-m", mode]);
        menuVisible = false;
    }

    // Region-only for v1 — a single wf-recorder process at a time, stopped
    // via SIGINT (see stopRecording()).
    function startRecording() {
        Quickshell.execDetached(["sh", "-c",
            'wf-recorder -g "$(slurp)" -f ~/Videos/screenrecording-$(date +%Y%m%d-%H%M%S).mp4']);
        recording = true;
        menuVisible = false;
    }

    // SIGINT lets wf-recorder finalize the mp4 instead of leaving it
    // corrupt, same as a manual Ctrl+C.
    function stopRecording() {
        Quickshell.execDetached(["pkill", "-INT", "wf-recorder"]);
        recording = false;
    }

    function ocrRegion() {
        Quickshell.execDetached(["sh", "-c", 'grim -g "$(slurp)" - | tesseract - - | wl-copy']);
        menuVisible = false;
    }
}
