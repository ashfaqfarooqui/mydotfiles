pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Replaces cliphist-picker.py's data layer (ALT+V). Adapted from the
// canonical services/Cliphist.qml used across the QuickShell community
// (end-4/dots-hyprland and its many forks — found via `gh search code
// "cliphist" language:qml`): plain `cliphist list`/`decode`/`delete`/`wipe`
// process calls, no custom scripting needed. `entryIsImage` matches
// cliphist's own "[[ binary data ... WxH ]]" placeholder line format.
Singleton {
    id: root

    property list<string> entries: []

    function entryIsImage(entry) {
        return /^\d+\t\[\[.*binary data.*\d+x\d+.*\]\]$/.test(entry);
    }

    // Thumbnail cache: once an id's decode finishes, resolving it again
    // (e.g. scrolling a row back into view) is a plain JS map lookup, not a
    // new subprocess — ListView recreates offscreen delegates as you scroll,
    // and without this every recreation re-spawned `sh -c` just to check
    // whether the cache file already existed on disk, which is what made
    // scrolling feel slow.
    property var thumbCache: ({})
    property var _thumbPending: ({})

    function thumbPath(entryId) {
        return Quickshell.env("XDG_RUNTIME_DIR") + "/cliphist-thumbs/" + entryId;
    }

    function requestThumb(entryId) {
        if (root.thumbCache[entryId] || root._thumbPending[entryId]) return;
        root._thumbPending[entryId] = true;
        const path = root.thumbPath(entryId);
        const proc = thumbProcComponent.createObject(root, {
            command: ["sh", "-c", "mkdir -p \"$(dirname '" + path + "')\" && [ -f '" + path + "' ] || cliphist decode " + entryId + " > '" + path + "'"]
        });
        proc.exited.connect(exitCode => {
            delete root._thumbPending[entryId];
            if (exitCode === 0) {
                const cache = Object.assign({}, root.thumbCache);
                cache[entryId] = "file://" + path;
                root.thumbCache = cache;
            }
            proc.destroy();
        });
        proc.running = true;
    }

    // Full-text cache: mirrors thumbCache above, but for the detail-pane
    // preview of a text entry — decode once per id, then it's a plain JS
    // map lookup on every subsequent highlight of the same row.
    property var fullTextCache: ({})
    property var _fullTextPending: ({})

    function requestFullText(entryId) {
        if (root.fullTextCache[entryId] || root._fullTextPending[entryId]) return;
        root._fullTextPending[entryId] = true;
        const proc = fullTextProcComponent.createObject(root, {
            command: ["cliphist", "decode", entryId]
        });
        proc.exited.connect(exitCode => {
            delete root._fullTextPending[entryId];
            if (exitCode === 0) {
                const cache = Object.assign({}, root.fullTextCache);
                cache[entryId] = proc.buffer.join("\n");
                root.fullTextCache = cache;
            }
            proc.destroy();
        });
        proc.running = true;
    }

    function refresh() {
        listProc.buffer = [];
        listProc.running = true;
    }

    function copy(entry) {
        const id = entry.split("\t", 1)[0];
        Quickshell.execDetached(["sh", "-c", "cliphist decode " + id + " | wl-copy"]);
    }

    // POSIX single-quote escaping: inside '...', every byte is literal
    // except a literal single quote, so this is safe against $()/``/\
    // clipboard content — unlike naive string interpolation into `sh -c`.
    function _shQuote(s) {
        return "'" + s.replace(/'/g, "'\\''") + "'";
    }

    function deleteEntry(entry) {
        deleteProc.entry = entry;
        deleteProc.command = ["sh", "-c", "echo " + root._shQuote(entry) + " | cliphist delete"];
        deleteProc.running = true;
    }

    function wipe() {
        wipeProc.running = true;
    }

    Component.onCompleted: refresh()

    Process {
        id: listProc
        property list<string> buffer: []
        command: ["cliphist", "list"]
        stdout: SplitParser {
            onRead: line => listProc.buffer.push(line)
        }
        onExited: exitCode => {
            if (exitCode === 0) root.entries = listProc.buffer;
        }
    }

    Process {
        id: deleteProc
        property string entry: ""
        onExited: root.refresh()
    }

    Process {
        id: wipeProc
        command: ["cliphist", "wipe"]
        onExited: root.refresh()
    }

    Component {
        id: thumbProcComponent
        Process {}
    }

    Component {
        id: fullTextProcComponent
        Process {
            id: fullTextProc
            property var buffer: []
            stdout: SplitParser {
                onRead: line => fullTextProc.buffer.push(line)
            }
        }
    }
}
