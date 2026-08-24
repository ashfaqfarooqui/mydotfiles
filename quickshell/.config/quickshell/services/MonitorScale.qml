pragma Singleton
import Quickshell

// Backs DisplayPanel.qml's Scale section. No first-party Quickshell service
// sets monitor scale (services/Hypr.qml's Hypr.monitors is read-only), so
// this shells out to `hyprctl keyword monitor` directly — the same
// mechanism basecamp/omarchy's own omarchy-hyprland-monitor-scaling CLI
// uses under the hood (confirmed via their quattro-branch Model.js/script).
// Live-apply only: no rewrite of hypr/.config/hypr/conf/monitors.lua, so a
// picked scale reverts to whatever monitors.lua says on the next Hyprland
// restart — same as any other ad-hoc `hyprctl keyword` change.
Singleton {
    id: root

    // Omarchy's own preset list (SCALES=(1 1.25 1.6 2 3 4) in
    // omarchy-hyprland-monitor-scaling).
    readonly property var rawPresets: [1, 1.25, 1.6, 2, 3, 4]

    // Hyprland requires a monitor's logical size (mode / scale) to land on
    // a whole number of 1/120th-pixel units — round(width * 120 / scale)
    // must be an integer, or it silently snaps to the nearest value that
    // is. Round each raw preset up to the nearest scale satisfying that for
    // this monitor's mode, then dedupe consecutive presets that land on the
    // same effective value (e.g. on some modes 1.25x and 1.6x round to the
    // same effective scale).
    function cleanScale(monitor, rawScale) {
        if (!monitor || monitor.width <= 0) return rawScale;
        const unit = monitor.width * 120;
        let candidate = rawScale;
        // Search outward in small steps for the nearest scale (rounding up
        // first, matching Omarchy's own snap-up behavior) where the mode
        // divides evenly into 1/120 logical-pixel units.
        for (let step = 0; step < 200; step++) {
            const testUp = rawScale + step * 0.01;
            if (Math.round(unit / testUp) === unit / testUp || Math.abs(unit / testUp - Math.round(unit / testUp)) < 0.01) {
                candidate = Math.round(testUp * 100) / 100;
                break;
            }
        }
        return candidate;
    }

    function scalePresetsFor(monitor) {
        if (!monitor) return [];
        const seen = new Set();
        const result = [];
        for (const raw of root.rawPresets) {
            const effective = root.cleanScale(monitor, raw);
            const key = effective.toFixed(2);
            if (seen.has(key)) continue;
            seen.add(key);
            result.push(effective);
        }
        return result;
    }

    function setScale(monitor, scale) {
        if (!monitor) return;
        Quickshell.execDetached(["hyprctl", "keyword", "monitor", monitor.name + ",preferred,auto," + scale]);
    }
}
