pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs.services

// Native Tailscale status/toggle/peer-browser/exit-node/Taildrop, ported down
// from basecamp/omarchy's shell/plugins/panels/tailscale to this repo's
// simpler singleton+panel convention. Not ported: account switching
// (multiple tailnets/profiles) — everything else (status, up/down with
// browser-auth handoff, online-peer list with IP/name/DNS copy, exit-node +
// Mullvad relay picker, Taildrop send) is here.
Singleton {
    id: root

    property bool installed: true
    property bool running: false
    property bool needsLogin: false
    property string backendState: "Unknown"
    property string statusText: "Checking…"
    property string selfName: ""
    property string selfIp: ""
    property string selfUserId: ""
    property bool fileSharing: false
    property var peers: []
    property string lastError: ""
    property string actionStatus: ""

    // This machine's tailscaled runs as root with no operator set, so the
    // CLI refuses `up`/`down`/`set --exit-node`/`file cp` with "Access
    // denied: ... Use 'sudo tailscale set --operator=$USER' once." — surface
    // an explicit authorize action instead of failing silently.
    property bool needsOperatorAuth: false

    function _checkAccessDenied(line) {
        if (/Access denied/i.test(String(line || ""))) root.needsOperatorAuth = true;
    }

    function authorizeOperator() {
        if (operatorProc.running) return;
        root.actionStatus = "Authorizing…";
        operatorProc.command = ["pkexec", "tailscale", "set", "--operator=" + (Quickshell.env("USER") || Quickshell.env("LOGNAME") || "")];
        operatorProc.running = true;
    }

    Process {
        id: operatorProc
        stderr: StdioCollector { id: operatorStderr }
        onExited: exitCode => {
            if (exitCode === 0) {
                root.needsOperatorAuth = false;
                root.actionStatus = "Operator authorized";
            } else {
                root.actionStatus = operatorStderr.text.trim() || "Authorization failed";
            }
            copyStatusTimer.restart();
            root.refresh();
        }
    }

    // Optimistic state so the toggle switch flips instantly instead of
    // waiting for the next poll — same trick MonitorScale.qml's refresh-on-
    // exit pattern is built around, just applied to a boolean instead of a
    // one-shot refresh.
    property int _desired: -1 // -1 = follow `running`, 0/1 = pending toggle
    readonly property bool active: _desired === -1 ? running : (_desired === 1)
    readonly property bool busy: statusProc.running || actionProc.running

    function filterIPv4(ips) {
        return (ips || []).filter(ip => /^100\./.test(String(ip || "")));
    }

    function cleanDnsName(name) {
        const value = String(name || "");
        return value.endsWith(".") ? value.slice(0, -1) : value;
    }

    function shortDnsName(name) {
        const clean = cleanDnsName(name);
        return clean === "" ? "" : (clean.split(".")[0] || clean);
    }

    function displayHostName(hostName, dnsName) {
        const host = String(hostName || "");
        if (host !== "" && host.toLowerCase() !== "localhost") return host;
        return shortDnsName(dnsName) || host || "Unknown";
    }

    function osIcon(os) {
        switch (String(os || "").toLowerCase()) {
        case "linux": return "\u{F033D}";
        case "macos": case "ios": return "\u{F0179}";
        case "windows": return "\u{F05B3}";
        case "android": return "\u{F0032}";
        default: return "\u{F0765}";
        }
    }

    // Address to hand to `tailscale set --exit-node=`/`tailscale file cp` —
    // prefer the stable DNS name over an IP that can be reassigned.
    function peerAddress(peer) {
        if (!peer) return "";
        if (peer.dnsName) return peer.dnsName;
        if (peer.hostName) return peer.hostName;
        return peer.ip || "";
    }

    // Tailscale grades every peer itself — offline, wrong owner, an OS
    // without Taildrop, no peer API — so take its word when the status
    // carries one (TaildropTarget: 1 = yes, other non-zero = no), and fall
    // back to same-owner for daemons too old to say.
    function isTaildropTarget(peer) {
        if (!peer) return false;
        if (typeof peer.taildropTarget === "number" && peer.taildropTarget !== 0) return peer.taildropTarget === 1;
        return peer.userId !== "" && peer.userId === root.selfUserId;
    }

    function canSendFiles(peer) {
        return root.fileSharing && root.running && isTaildropTarget(peer);
    }

    function refresh() {
        if (!statusProc.running) statusProc.running = true;
        if (!mullvadProc.running) mullvadProc.running = true;
    }

    // Taildrop is a tailnet feature the admin can turn off, so the send
    // button only makes sense when this profile actually carries the
    // capability.
    function hasFileSharing(self) {
        const capability = "https://tailscale.com/cap/file-sharing";
        const capMap = (self && self.CapMap) || null;
        if (capMap && capMap[capability] !== undefined) return true;
        const capabilities = (self && self.Capabilities) || [];
        return capabilities.some(c => String(c) === capability);
    }

    function parseStatus(raw) {
        const text = String(raw || "").trim();
        if (text === "") {
            root.running = false;
            root.needsLogin = false;
            root.backendState = "Unavailable";
            root.statusText = "Disconnected";
            root.peers = [];
            return;
        }
        try {
            const data = JSON.parse(text);
            const backendState = String(data.BackendState || "Unknown");
            const self = data.Self || {};
            const selfIps = filterIPv4(self.TailscaleIPs || data.TailscaleIPs || []);
            const rawPeers = data.Peer || {};
            const peers = [];
            let activeTailnetExitNode = null;
            for (const id in rawPeers) {
                const peer = rawPeers[id] || {};
                if (!peer.Online) continue;
                const normalized = {
                    id,
                    hostName: displayHostName(peer.HostName, peer.DNSName),
                    ip: filterIPv4(peer.TailscaleIPs || [])[0] || "",
                    dnsName: cleanDnsName(peer.DNSName || ""),
                    os: String(peer.OS || ""),
                    online: peer.Online === true,
                    userId: String(peer.UserID || ""),
                    taildropTarget: typeof peer.TaildropTarget === "number" ? peer.TaildropTarget : 0,
                    exitNodeOption: peer.ExitNodeOption === true,
                    exitNode: peer.ExitNode === true
                };
                peers.push(normalized);
                if (normalized.exitNode) activeTailnetExitNode = normalized;
            }
            peers.sort((a, b) => a.hostName.localeCompare(b.hostName));

            root.backendState = backendState;
            root.running = backendState === "Running";
            if (root._desired !== -1 && root.running === (root._desired === 1)) root._desired = -1;
            root.needsLogin = backendState === "NeedsLogin";
            root.selfName = displayHostName(self.HostName, self.DNSName);
            root.selfIp = selfIps[0] || "";
            root.selfUserId = String(self.UserID || "");
            root.fileSharing = hasFileSharing(self);
            root.peers = peers;
            root.statusText = root.needsLogin ? "Needs login" : (root.running ? "Connected" : (backendState === "Stopped" ? "Disconnected" : backendState));
            root.lastError = "";
            root._activeTailnetExitNode = activeTailnetExitNode;
        } catch (e) {
            root.backendState = "Unavailable";
            root.statusText = "Status error";
            root.peers = [];
            root.lastError = "Failed to parse tailscale status";
        }
    }

    // --- Exit node / Mullvad relay picker ---
    // `tailscale exit-node list` is a fixed-width table, not JSON — sliced by
    // column start offsets read from its own header line, same approach as
    // Omarchy's Model.js parseExitNodeList.
    property var _activeTailnetExitNode: null
    property var mullvadNodes: [] // deduped one-per-city, from parseMullvadTable
    readonly property var tailnetExitNodeOptions: peers.filter(p => p.exitNodeOption)
    readonly property var exitNodeOptions: tailnetExitNodeOptions.concat(mullvadNodes)
    readonly property var activeExitNode: _activeTailnetExitNode ?? (mullvadNodes.find(n => n.active) ?? null)
    readonly property string activeExitNodeLabel: activeExitNode ? activeExitNode.hostName : "None"

    function _sliceColumn(line, start, end) {
        const text = String(line || "");
        if (start < 0 || start >= text.length) return "";
        return (end < 0 ? text.substring(start) : text.substring(start, Math.min(end, text.length))).trim();
    }

    function parseMullvadTable(raw) {
        const lines = String(raw || "").split(/\r?\n/);
        let headerIndex = -1;
        for (let i = 0; i < lines.length; i++) {
            if (/^\s*IP\s+HOSTNAME\s+COUNTRY\s+CITY\s+STATUS\s*$/.test(lines[i])) { headerIndex = i; break; }
        }
        if (headerIndex === -1) return [];

        const header = lines[headerIndex];
        const ipStart = header.indexOf("IP");
        const hostStart = header.indexOf("HOSTNAME");
        const countryStart = header.indexOf("COUNTRY");
        const cityStart = header.indexOf("CITY");
        const statusStart = header.indexOf("STATUS");
        const byCity = {};

        for (let j = headerIndex + 1; j < lines.length; j++) {
            const line = lines[j];
            if (/^\s*$/.test(line) || /^\s*#/.test(line)) continue;
            const ip = _sliceColumn(line, ipStart, hostStart);
            const host = _sliceColumn(line, hostStart, countryStart);
            const country = _sliceColumn(line, countryStart, cityStart);
            const city = _sliceColumn(line, cityStart, statusStart);
            const status = _sliceColumn(line, statusStart, -1);
            if (!/\.mullvad\.ts\.net$/i.test(host) || city === "" || city === "Any") continue;

            const key = country + "\n" + city;
            if (byCity[key]) continue;
            byCity[key] = {
                kind: "mullvad",
                id: "mullvad:" + key,
                hostName: city + ", " + country,
                dnsName: host,
                ip,
                active: status !== "" && status !== "-"
            };
        }
        return Object.values(byCity).sort((a, b) => a.hostName.localeCompare(b.hostName));
    }

    function setExitNode(node) {
        if (exitNodeProc.running) return;
        const target = node ? peerAddress(node) : "";
        exitNodeProc.command = ["tailscale", "set", "--exit-node=" + target];
        exitNodeProc.running = true;
    }

    // --- Taildrop send ---
    function sendFile(peer) {
        if (!canSendFiles(peer) || filePickerProc.running) return;
        _sendTarget = peerAddress(peer);
        filePickerProc.running = true;
    }

    property string _sendTarget: ""

    Process {
        id: filePickerProc
        command: ["zenity", "--file-selection", "--title=Send file via Taildrop"]
        stdout: StdioCollector {
            onStreamFinished: {
                const path = this.text.trim();
                if (path === "") return;
                root.actionStatus = "Sending…";
                sendProc.command = ["tailscale", "file", "cp", path, root._sendTarget + ":"];
                sendProc.running = true;
            }
        }
    }

    Process {
        id: sendProc
        stderr: SplitParser { onRead: line => root._checkAccessDenied(line) }
        onExited: exitCode => {
            root.actionStatus = exitCode === 0 ? "Sent" : (root.needsOperatorAuth ? "Send failed — authorize operator first" : "Send failed");
            copyStatusTimer.restart();
        }
    }

    // Set true only while an in-flight `up` is watching its own output for
    // the auth URL — a plain `down` never needs this, and re-arming it on
    // every actionProc run would let a stray "https://" in unrelated output
    // (there isn't one today, but it's cheap insurance) open a browser.
    property bool _awaitingAuthUrl: false
    property bool _authUrlOpened: false

    function _tryOpenAuthUrl(text) {
        if (root._authUrlOpened) return;
        const match = String(text || "").match(/https:\/\/login\.tailscale\.com\S*/);
        if (!match) return;
        root._authUrlOpened = true;
        // Stop pretending we're up — turning on ended up needing browser
        // auth, so the toggle switch should reflect that we're still off
        // until the user actually completes login.
        root._desired = -1;
        root.actionStatus = "Opening login in browser…";
        Quickshell.execDetached(["xdg-open", match[0]]);
    }

    function toggle() {
        if (busy) return;
        if (active) {
            root._desired = 0;
            root._awaitingAuthUrl = false;
            actionProc.command = ["tailscale", "down"];
        } else {
            root._desired = 1;
            root._awaitingAuthUrl = true;
            root._authUrlOpened = false;
            actionProc.command = ["tailscale", "up"];
        }
        actionProc.running = true;
    }

    function copyToClipboard(value) {
        const text = String(value || "");
        if (text === "") return;
        Quickshell.execDetached(["wl-copy", text]);
        root.actionStatus = "Copied";
        copyStatusTimer.restart();
    }

    function copyPeerIp(peer) {
        if (peer) copyToClipboard(peer.ip);
    }

    function copyPeerName(peer) {
        if (peer) copyToClipboard(peer.hostName);
    }

    function copyPeerDnsName(peer) {
        if (peer) copyToClipboard(peer.dnsName);
    }

    Timer {
        interval: 30000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }

    Timer {
        id: copyStatusTimer
        interval: 1500
        onTriggered: root.actionStatus = ""
    }

    Process {
        id: statusProc
        command: ["tailscale", "status", "--json"]
        stdout: StdioCollector {
            onStreamFinished: root.parseStatus(this.text)
        }
        stderr: StdioCollector {
            onStreamFinished: {
                if (this.text.trim() !== "" && !statusProc.running) root.lastError = this.text.trim();
            }
        }
        onExited: exitCode => {
            if (exitCode !== 0 && this.stdout.text === "") {
                root.running = false;
                root.needsLogin = false;
                root.backendState = "Unavailable";
                root.statusText = "Not installed";
                root.installed = false;
                root.peers = [];
            }
        }
    }

    Process {
        id: mullvadProc
        command: ["tailscale", "exit-node", "list"]
        stdout: StdioCollector {
            onStreamFinished: root.mullvadNodes = root.parseMullvadTable(this.text)
        }
        onExited: exitCode => { if (exitCode !== 0) root.mullvadNodes = []; }
    }

    Process {
        id: exitNodeProc
        stderr: SplitParser { onRead: line => root._checkAccessDenied(line) }
        onExited: exitCode => {
            if (exitCode !== 0) {
                root.actionStatus = root.needsOperatorAuth ? "Authorize operator to set an exit node" : "Exit node selection failed";
                copyStatusTimer.restart();
            }
            root.refresh();
        }
    }

    Process {
        id: actionProc
        // `tailscale up` prints the login URL to stderr once it needs
        // interactive auth (first login, or after `tailscale logout`) and
        // then blocks until the browser flow completes — SplitParser catches
        // it line-by-line instead of waiting for the whole process to exit.
        stdout: SplitParser { onRead: line => { if (root._awaitingAuthUrl) root._tryOpenAuthUrl(line); } }
        stderr: SplitParser { onRead: line => { if (root._awaitingAuthUrl) root._tryOpenAuthUrl(line); root._checkAccessDenied(line); } }
        onExited: exitCode => {
            root._awaitingAuthUrl = false;
            if (exitCode !== 0 && !root._authUrlOpened) {
                root._desired = -1;
                root.actionStatus = root.needsOperatorAuth ? "Authorize operator to use Tailscale" : "Tailscale command failed";
                copyStatusTimer.restart();
            } else if (!root._authUrlOpened) {
                root.actionStatus = "";
            }
            root.refresh();
        }
    }

    // Panel visibility, same singleton-flag pattern as Network.qml/Bluetooth.qml.
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
            AgentsUsage.hidePanel();
            Weather.hidePanel();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }
}
