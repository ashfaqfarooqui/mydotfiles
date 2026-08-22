pragma Singleton
import Quickshell
import Quickshell.Services.Pipewire
import qs.config

// Replaces waybar's privacy indicator module (screenshare/audio-in/audio-out
// icons). PwNodeType.AudioInStream (Audio|Source|Stream) is specifically a
// recording stream — an app's playback (AudioOutStream, Audio|Sink|Stream)
// is deliberately excluded so this doesn't light up for every Spotify/
// browser tab playing audio, only genuine mic capture. Video is not split
// into camera vs screenshare at the Pipewire level the same way, so both
// map to one "video in use" signal (covers webcam + portal screen-share).
Singleton {
    readonly property var nodes: {
        const list = Pipewire.nodes.values;
        return list.filter(n => n.isStream &&
            !Config.privacyIgnore.some(ignored => (n.name ?? "").toLowerCase().includes(ignored)));
    }

    readonly property bool micInUse: nodes.some(n =>
        (n.type & PwNodeType.Audio) && (n.type & PwNodeType.Source) && (n.type & PwNodeType.Stream))

    readonly property bool videoInUse: nodes.some(n => n.type & PwNodeType.Video)
}
