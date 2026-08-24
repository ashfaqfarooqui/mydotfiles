pragma Singleton
import QtQuick
import Quickshell

// Relative-time formatting ("2m ago") for notification history rows.
// `tick` exists purely to force re-binding on a wall clock: QML has no
// implicit dependency between a plain JS function call and time passing, so
// a Text.text binding that only calls TimeFormat.relative(x) would compute
// once and then sit stale forever — referencing `tick` inside the binding
// (even multiplied by zero) makes it a real property dependency.
Singleton {
    id: root

    property int tick: 0

    Timer {
        interval: 60000
        running: true
        repeat: true
        onTriggered: root.tick++
    }

    function relative(epochMs) {
        const diffSec = Math.max(0, Math.floor((Date.now() - epochMs) / 1000));
        if (diffSec < 60) return "just now";
        const diffMin = Math.floor(diffSec / 60);
        if (diffMin < 60) return diffMin + "m ago";
        const diffHour = Math.floor(diffMin / 60);
        if (diffHour < 24) return diffHour + "h ago";
        const diffDay = Math.floor(diffHour / 24);
        return diffDay + "d ago";
    }
}
