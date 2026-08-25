pragma Singleton
import Quickshell
import Quickshell.Services.Pipewire
import qs.services

// Replaces waybar's pulseaudio module + volume-control.sh's wpctl calls.
Singleton {
    id: root

    // Every node this shell touches must stay listed here — PwObjectTracker
    // is what keeps a node's PwNodeAudio bound (unbound nodes report stale
    // volume/muted), confirmed via
    // https://quickshell.org/docs/types/Quickshell.Services.Pipewire/Pipewire/.
    // Sinks/sources/streams are re-derived below, so this list follows them.
    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource, ...root.outputDevices, ...root.inputDevices, ...root.streams].filter(n => n !== null)
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

    readonly property real sourceVolume: source?.audio?.volume ?? 0
    readonly property int sourceVolumePercent: Math.round(sourceVolume * 100)

    function setSourceVolume(v) {
        if (source?.audio) source.audio.volume = Math.max(0, Math.min(1.5, v));
    }

    // Hardware/virtual sink and source devices (not app streams), for
    // VolumePanel.qml's output/input device pickers. PwNode.isStream is
    // false for a hardware device, true for a program — confirmed via
    // https://quickshell.org/docs/types/Quickshell.Services.Pipewire/PwNode.
    readonly property var outputDevices: Pipewire.nodes.values.filter(n => n.isSink && !n.isStream)
    readonly property var inputDevices: Pipewire.nodes.values.filter(n => !n.isSink && !n.isStream && n.audio)

    // Per-application audio streams, for VolumePanel.qml's mixer rows.
    readonly property var streams: Pipewire.nodes.values.filter(n => n.isStream && n.audio)

    function setOutputDevice(node) {
        Pipewire.preferredDefaultAudioSink = node;
    }

    function setInputDevice(node) {
        Pipewire.preferredDefaultAudioSource = node;
    }

    function streamLabel(node) {
        return node?.properties?.["application.name"] ?? node?.description ?? node?.name ?? "";
    }

    // Visibility for VolumePanel.qml (modules/bar/) — same
    // toggle-flag-on-the-singleton pattern as Network.qml/Bluetooth.qml/
    // Battery.qml/Brightness.qml, see Network.qml for why the per-screen
    // gating is mandatory.
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            Network.hidePanel();
            Bluetooth.hidePanel();
            Battery.hidePanel();
            Brightness.hidePanel();
            Calendar.hidePanel();
            SystemStats.hidePanel();
            AgentsUsage.hidePanel();
            Tailscale.hidePanel();
            Weather.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }
}
