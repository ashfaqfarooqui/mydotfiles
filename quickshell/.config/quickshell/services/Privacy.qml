pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Pipewire
import qs.config

// Replaces waybar's privacy indicator module (screenshare/audio-in/audio-out
// icons). PwNodeType.AudioInStream (Audio|Source|Stream) is specifically a
// recording stream — an app's playback (AudioOutStream, Audio|Sink|Stream)
// is deliberately excluded so this doesn't light up for every Spotify/
// browser tab playing audio, only genuine mic capture.
//
// Video is NOT handled the same way as mic, because camera and screen-share
// reach PipeWire through two different paths, neither of which produces a
// classifiable *stream* node the way audio does — confirmed empirically
// with a real webcam (gst-launch/ffmpeg) and a real Teams screen-share, by
// spinning up a throwaway `quickshell -p` instance and dumping
// PwNode.isStream/.type live (Quickshell's Pipewire docs don't cover this):
//  - A webcam is exposed once as a permanent Video/Source *device* node
//    (isStream: false — pw-dump showed "v4l2_input.pci-..." with
//    media.class "Video/Source", not a Stream), and most consumers
//    (browsers doing getUserMedia, in particular) open /dev/video* via V4L2
//    directly, never touching PipeWire at all — so no stream node, and no
//    PipeWire link, ever appears for them.
//  - xdg-desktop-portal screen-share: the *consuming* app's node ("Stream/
//    Input/Video", e.g. name "teams-for-linux" while sharing in Teams) came
//    back isStream: false, type: 0 (Untracked) — Quickshell's PwNodeType
//    parser doesn't recognize generic video stream classes at all, only the
//    literal "Video/Source"/"Video/Sink" device classes (confirmed:
//    PwNodeType.VideoSource === Video|Source === 10, and the portal's own
//    node showed up as exactly type 10). What *does* appear only while
//    sharing is the portal's own ephemeral Video/Source node (name
//    "xdg-desktop-portal-hyprland" in this case) — ephemeral because the
//    portal opens it per screencast session and tears it down when the
//    share ends, unlike the webcam's permanent "v4l2_input..." device node.
//    That's what screenshareInUse below keys off, filtered to the
//    "xdg-desktop-portal" name prefix specifically so it doesn't also fire
//    for the webcam device node (same Video|Source type, different name).
//
// One regression from this: because the signal is the shared portal node,
// not a per-consumer stream, Config.privacyIgnore (the "obs" entry) can no
// longer distinguish "OBS is compositing locally" from "OBS is the thing
// actually screen-sharing" — both look identical from the source side.
Singleton {
    id: root

    readonly property var nodes: {
        const list = Pipewire.nodes.values;
        return list.filter(n => n.isStream &&
            !Config.privacyIgnore.some(ignored => (n.name ?? "").toLowerCase().includes(ignored)));
    }

    readonly property bool micInUse: nodes.some(n =>
        (n.type & PwNodeType.Audio) && (n.type & PwNodeType.Source) && (n.type & PwNodeType.Stream))

    readonly property bool screenshareInUse: Pipewire.nodes.values.some(n =>
        (n.type & PwNodeType.Video) && (n.type & PwNodeType.Source) &&
        (n.name ?? "").startsWith("xdg-desktop-portal"))

    property bool cameraInUse: false

    readonly property bool videoInUse: screenshareInUse || cameraInUse

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: cameraProc.running = true
    }

    Process {
        id: cameraProc
        // fuser exits 0 (prints holders) only when at least one of the
        // device nodes is open; a plain glob (no matches) or nothing open
        // both exit non-zero with empty stdout, which is fine either way.
        command: ["sh", "-c", "fuser /dev/video* 2>/dev/null"]
        stdout: StdioCollector {
            onStreamFinished: root.cameraInUse = this.text.trim().length > 0
        }
    }
}
