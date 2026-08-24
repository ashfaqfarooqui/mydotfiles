pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces waybar's "custom/voxtype" module (voxtype-module.jsonc). That
// module runs `voxtype status --format json --follow` and reads each JSON
// line's "class"/"tooltip" fields (confirmed live: {"text": "🎙️", "alt":
// "idle", "class": "idle", "tooltip": "..."}); "class" is voxtype's own
// normalized state and matches the states styled in waybar's style.css
// (#custom-voxtype.idle/.transcribing/.recording/.stopped), so it's reused
// verbatim as `state` here instead of re-deriving one from "text"/"alt".
Singleton {
    id: root

    property string state: "idle"
    property string tooltip: "Voxtype ready"

    Process {
        running: true
        command: ["voxtype", "status", "--format", "json", "--follow"]
        stdout: SplitParser {
            onRead: line => {
                if (!line || !line.trim().length) return;
                try {
                    const data = JSON.parse(line);
                    if (data.class) root.state = data.class;
                    if (data.tooltip) root.tooltip = data.tooltip;
                } catch (e) {
                    // A partial line can arrive mid-write; just skip it and
                    // wait for the next complete one.
                }
            }
        }
    }
}
