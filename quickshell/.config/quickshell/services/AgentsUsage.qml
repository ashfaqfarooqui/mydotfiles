pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

// Local-only "Agents" usage panel data, for two sources — see
// scripts/agents-usage.py for the aggregation itself. Claude Code reads
// ~/.claude/projects transcripts rather than the (stale, non-live)
// ~/.claude/usage-data/session-meta snapshots; opencode reads via its own
// `opencode db` CLI (not the sqlite file directly). No Session/Weekly quota
// % bars for Claude Code — that needs Anthropic's undocumented OAuth usage
// endpoint, scoped out as unsupported/fragile; opencode has no equivalent.
Singleton {
    id: root

    // "claude" or "opencode" — which source the panel is currently showing.
    property string source: "claude"

    property var claudeByDay: [] // [{day, tokens}]
    property var claudeByModel: [] // [{model, tokens}]
    property var opencodeByDay: []
    property var opencodeByModel: []

    readonly property var tokensByDay: source === "claude" ? claudeByDay : opencodeByDay
    readonly property var tokensByModel: source === "claude" ? claudeByModel : opencodeByModel

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
            Audio.hidePanel();
            Calendar.hidePanel();
            SystemStats.hidePanel();
            Tailscale.hidePanel();
            Weather.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
            refresh();
        }
    }

    function hidePanel() {
        panelVisible = false;
    }

    function refresh() {
        proc.running = true;
    }

    // Background refresh every 5 minutes, plus on panel open above — the
    // script itself is fast (~0.3s for this machine's transcript volume)
    // but there's no need to run it more often than that.
    Timer {
        interval: 300000
        running: true
        repeat: true
        onTriggered: root.refresh()
    }

    Process {
        id: proc
        command: ["python3", Quickshell.env("HOME") + "/.config/quickshell/scripts/agents-usage.py"]
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    const data = JSON.parse(this.text);
                    root.claudeByDay = data.claude?.tokensByDay ?? [];
                    root.claudeByModel = data.claude?.tokensByModel ?? [];
                    root.opencodeByDay = data.opencode?.tokensByDay ?? [];
                    root.opencodeByModel = data.opencode?.tokensByModel ?? [];
                } catch (e) {
                    // leave last-known-good data in place
                }
            }
        }
    }

    Component.onCompleted: refresh()
}
