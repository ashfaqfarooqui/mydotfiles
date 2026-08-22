pragma Singleton
import Quickshell
import Quickshell.Services.Pipewire

// Replaces waybar's pulseaudio module + volume-control.sh's wpctl calls.
Singleton {
    id: root

    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource].filter(n => n !== null)
    }

    readonly property var sink: Pipewire.defaultAudioSink
    readonly property var source: Pipewire.defaultAudioSource

    readonly property real volume: sink?.audio?.volume ?? 0
    readonly property bool muted: sink?.audio?.muted ?? true
    readonly property int volumePercent: Math.round(volume * 100)

    function setVolume(v) {
        if (sink?.audio) sink.audio.volume = Math.max(0, Math.min(1.5, v));
    }

    function stepVolume(deltaPercent) {
        setVolume(volume + deltaPercent / 100);
    }

    function toggleMute() {
        if (sink?.audio) sink.audio.muted = !sink.audio.muted;
    }

    readonly property bool sourceMuted: source?.audio?.muted ?? true

    function toggleSourceMute() {
        if (source?.audio) source.audio.muted = !source.audio.muted;
    }
}
