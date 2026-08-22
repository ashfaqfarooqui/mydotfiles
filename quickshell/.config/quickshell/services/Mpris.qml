pragma Singleton
import Quickshell
import Quickshell.Services.Mpris as QM

// Replaces waybar's mpris module + media-player.py's manual playerctl
// polling. Aliased import ("as QM") because this file's own type name is
// also "Mpris" (same-directory auto-registration) — an unaliased import of
// the module's own "Mpris" singleton would collide with that and silently
// resolve to the wrong thing, which is very likely why media info never
// showed up in the bar before this fix.
Singleton {
    readonly property var players: QM.Mpris.players

    // First actively-playing player, else the first available one — mirrors
    // playerctl's default "most relevant player" behavior closely enough
    // for a single-line bar widget.
    readonly property var activePlayer: {
        const list = players.values;
        for (let i = 0; i < list.length; i++) {
            if (list[i].playbackState === QM.MprisPlaybackState.Playing) return list[i];
        }
        return list.length > 0 ? list[0] : null;
    }

    function playPause() {
        if (!activePlayer) return;
        if (activePlayer.playbackState === QM.MprisPlaybackState.Playing) activePlayer.pause();
        else activePlayer.play();
    }

    function next() {
        activePlayer?.next();
    }

    function previous() {
        activePlayer?.previous();
    }
}
