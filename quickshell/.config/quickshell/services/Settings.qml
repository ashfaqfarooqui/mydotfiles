pragma Singleton
import Quickshell
import Quickshell.Io

// The one writable settings file in this shell. Every other "persisted"
// value in the repo is either static (config/Config.qml, hand-edited) or
// one-way external-tool-writes/QML-reads (theme/theme.json, written by
// `just apply`) — this is the first thing QML itself needs to write back to
// disk, for the clock format and lock-screen appearance pickers.
// JsonAdapter/FileView.writeAdapter() confirmed via
// https://quickshell.org/docs/types/Quickshell.Io/JsonAdapter/ — each
// JsonAdapter property maps 1:1 to a JSON key, and onAdapterUpdated:
// writeAdapter() is the documented auto-persist pattern.
Singleton {
    id: root

    property alias clockFormat: adapter.clockFormat
    property alias weekStartsMonday: adapter.weekStartsMonday
    property alias lockWallpaper: adapter.lockWallpaper
    property alias lockBlur: adapter.lockBlur
    property alias fontSize: adapter.fontSize

    FileView {
        id: file
        path: Quickshell.env("HOME") + "/.config/quickshell/settings.json"
        watchChanges: true
        printErrors: false

        // First run: no settings.json exists yet on disk. FileNotFound just
        // means "use the adapter's defaults and create the file now."
        onLoadFailed: error => {
            if (error === FileViewError.FileNotFound) writeAdapter();
        }

        JsonAdapter {
            id: adapter
            property string clockFormat: "dddd HH:mm"
            property bool weekStartsMonday: true
            property string lockWallpaper: Quickshell.env("HOME") + "/mydotfiles/Wallpapers/DSC_0749-1.jpg"
            property real lockBlur: 64
            // Bar-row text size — a single sensible default, not a per-
            // monitor setting. Qt's fractional-scaling already multiplies
            // this by each output's own Hyprland scale factor, so it reads
            // consistently across monitors on its own; per-monitor DPI is
            // controlled via DisplayPanel.qml's SCALE section instead.
            // 12, not 13, so it lands exactly on one of DisplayPanel.qml's
            // TEXT SIZE slider stops ([9,10,11,12,14,16,20]) by default.
            property int fontSize: 12
        }

        onAdapterUpdated: writeAdapter()
    }
}
