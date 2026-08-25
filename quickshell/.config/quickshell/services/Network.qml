pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Networking as QN
import qs.services

// Replaces waybar's "network" module. Uses the native Quickshell.Networking
// binding (confirmed via https://quickshell.org/docs/v0.3.0/types/Quickshell.Networking/
// — Networking.devices is a live ObjectModel<NetworkDevice>, NetworkDevice.connected
// is a live bool, and WifiNetwork.signalStrength/Network.name update live) instead
// of polling nmcli on a Timer.
Singleton {
    id: root

    // Prefer a connected wired device over any other connected device — the
    // NetworkPanel header already assumed this priority (its own separate
    // `wiredDevice` lookup titles the panel "Ethernet" whenever a wired
    // device is connected), but this activeDevice pick used to take
    // whichever connected device came first in Networking.devices with no
    // type priority, so the header could say "Ethernet" while the stat
    // grid/QR/speedtest/band-toggle below it were still keyed off Wi-Fi (or
    // vice versa) whenever both were connected at once.
    readonly property var activeDevice: QN.Networking.devices.values.find(d => d.connected && d.type === QN.DeviceType.Wired)
        ?? QN.Networking.devices.values.find(d => d.connected)
        ?? null

    // Startup-only safety net: Networking.devices can take a beat to
    // populate if quickshell starts (via Hyprland exec-once) before
    // NetworkManager's D-Bus service is fully up, which otherwise leaves
    // `kind` stuck on "disconnected" until something else nudges the
    // native binding. A few cheap nmcli checks bridge that gap; once the
    // native binding populates (or the retries run out) this fallback
    // gets out of the way entirely — `kind` goes back to being driven
    // solely by activeDevice, same as before.
    property bool _startupFallbackActive: true
    property string _startupFallbackKind: ""
    property int _startupFallbackAttempts: 0

    readonly property string kind: activeDevice !== null
        ? (activeDevice.type === QN.DeviceType.Wifi ? "wifi" : "ethernet")
        : (_startupFallbackActive && _startupFallbackKind !== "" ? _startupFallbackKind : "disconnected")

    onActiveDeviceChanged: if (activeDevice !== null) root._startupFallbackActive = false

    Timer {
        id: startupFallbackTimer
        interval: 700
        repeat: true
        running: root._startupFallbackActive
        triggeredOnStart: true
        onTriggered: {
            root._startupFallbackAttempts++;
            if (root._startupFallbackAttempts > 5) {
                root._startupFallbackActive = false;
                return;
            }
            startupFallbackProc.buffer = [];
            startupFallbackProc.running = true;
        }
    }

    Process {
        id: startupFallbackProc
        property list<string> buffer: []
        command: ["nmcli", "-t", "-f", "DEVICE,TYPE,STATE", "device", "status"]
        stdout: SplitParser {
            onRead: line => startupFallbackProc.buffer.push(line)
        }
        onExited: exitCode => {
            if (exitCode !== 0 || !root._startupFallbackActive) return;
            const wired = startupFallbackProc.buffer.some(l => l.split(":")[1] === "ethernet" && l.split(":")[2] === "connected");
            const wifi = startupFallbackProc.buffer.some(l => l.split(":")[1] === "wifi" && l.split(":")[2] === "connected");
            root._startupFallbackKind = wired ? "ethernet" : (wifi ? "wifi" : "");
        }
    }

    readonly property var activeWifiNetwork: (activeDevice !== null && activeDevice.type === QN.DeviceType.Wifi)
        ? activeDevice.networks.values.find(n => n.connected) ?? null
        : null

    readonly property string ssid: activeWifiNetwork?.name ?? ""
    // WifiNetwork.signalStrength is 0.0-1.0; keep signalStrength as a 0-100
    // int to match the previous nmcli-based contract bar widgets already use.
    readonly property int signalStrength: Math.round((activeWifiNetwork?.signalStrength ?? 0) * 100)

    readonly property string ifaceName: activeDevice?.name ?? ""

    // NetworkDevice.address (checked against quickshell-network.qmltypes)
    // only exposes the device's own IPv4/CIDR — no gateway, no VPN/route
    // info, and Network.nmSettings is a QList<NMSettings*> with no QML-side
    // properties exported at all (isCreatable: false, no Property entries),
    // so a connection's actual name isn't readable from QML either. That's
    // why localIp/gatewayIp/vpnActive all fall back to nmcli here, and every
    // nmcli call below that needs "the active connection" uses ssid as its
    // id — nmcli auto-creates wifi connection profiles named after the SSID,
    // same assumption the password-prompt flow above already relies on.
    property string localIp: ""
    property string gatewayIp: ""
    property bool vpnActive: false

    function refreshIpInfo() {
        if (ifaceName === "") {
            localIp = "";
            gatewayIp = "";
            return;
        }
        ipInfoProc.buffer = [];
        ipInfoProc.command = ["nmcli", "-t", "-g", "IP4.ADDRESS,IP4.GATEWAY", "device", "show", ifaceName];
        ipInfoProc.running = true;
        vpnProc.buffer = [];
        vpnProc.running = true;
    }

    // Only refreshed when the panel opens, not polled — matches the
    // Cliphist.qml on-demand/no-Timer convention this repo uses throughout.
    // (Actively driving the Wi-Fi scanner itself is done by NetworkPanel.qml
    // directly on the WifiDevice it already holds a reference to — see the
    // comment there.)
    onPanelVisibleChanged: if (panelVisible) refreshIpInfo();

    Process {
        id: ipInfoProc
        property list<string> buffer: []
        stdout: SplitParser {
            onRead: line => ipInfoProc.buffer.push(line)
        }
        onExited: exitCode => {
            if (exitCode !== 0) return;
            // -g output is one value per requested field, in order.
            root.localIp = (ipInfoProc.buffer[0] ?? "").split("/")[0];
            root.gatewayIp = ipInfoProc.buffer[1] ?? "";
        }
    }

    Process {
        id: vpnProc
        property list<string> buffer: []
        command: ["nmcli", "-t", "-g", "TYPE", "connection", "show", "--active"]
        stdout: SplitParser {
            onRead: line => vpnProc.buffer.push(line)
        }
        onExited: exitCode => {
            if (exitCode !== 0) return;
            root.vpnActive = vpnProc.buffer.some(t => t === "vpn" || t === "wireguard");
        }
    }

    // POSIX single-quote escaping for values interpolated into `sh -c`
    // strings below — identical approach to Cliphist.qml's _shQuote.
    function _shQuote(s) {
        return "'" + s.replace(/'/g, "'\\''") + "'";
    }

    // bandAutomatic is a local UI flag, not read back from NetworkManager
    // (nmcli has no simple "is this pinned" query) — true until the user
    // explicitly pins a band via setBand(), so the panel's AUTOMATIC toggle
    // reflects the last action taken here rather than round-tripping state.
    property bool bandAutomatic: true

    function setBand(band) {
        const conn = root.ssid;
        if (conn === "") return;
        root.bandAutomatic = band === "auto";
        const value = band === "auto" ? "" : band;
        const cmd = "nmcli connection modify " + root._shQuote(conn) + " 802-11-wireless.band " + root._shQuote(value)
            + " && nmcli connection up " + root._shQuote(conn);
        Quickshell.execDetached(["sh", "-c", cmd]);
    }

    // Current AP's actual band (as opposed to setBand()'s pinning
    // preference) — read from `iw dev <iface> link`'s frequency, only while
    // the panel's open. `iw` isn't installed on every machine (confirmed
    // absent on this one) — probed once at startup rather than letting the
    // Process fail-and-warn on every 5s tick when it's missing.
    property string currentBandLabel: ""
    property bool _iwAvailable: false

    Process {
        running: true
        command: ["sh", "-c", "command -v iw"]
        onExited: exitCode => root._iwAvailable = exitCode === 0
    }

    Timer {
        interval: 5000
        running: root.panelVisible && root.kind === "wifi" && root._iwAvailable
        repeat: true
        triggeredOnStart: true
        onTriggered: if (root.ifaceName !== "") bandProc.running = true
    }

    Process {
        id: bandProc
        command: root.ifaceName !== "" ? ["iw", "dev", root.ifaceName, "link"] : ["true"]
        stdout: StdioCollector {
            onStreamFinished: {
                const m = /freq:\s*(\d+)/.exec(this.text);
                root.currentBandLabel = m ? (Number(m[1]) < 2500 ? "2.4GHz" : (Number(m[1]) < 5925 ? "5GHz" : "6GHz")) : "";
            }
        }
    }

    // Ping/packet loss, against the gateway ("router") and 1.1.1.1
    // ("internet") — deliberately not ported from Omarchy's 24-sample
    // smoothing, just a plain rolling window. Only runs while the panel is
    // open, matching refreshIpInfo()'s on-open-only convention.
    property real routerPingMs: -1
    property int routerLossPercent: 0
    property real internetPingMs: -1
    property int internetLossPercent: 0
    property var _routerSamples: []
    property var _internetSamples: []

    function _recordPing(kind, ok, ms) {
        const key = kind === "router" ? "_routerSamples" : "_internetSamples";
        const samples = root[key].slice();
        samples.push(ok ? ms : null);
        if (samples.length > 10) samples.shift();
        root[key] = samples;
        const valid = samples.filter(s => s !== null);
        const avgMs = valid.length > 0 ? valid.reduce((a, b) => a + b, 0) / valid.length : -1;
        const lossPercent = Math.round((1 - valid.length / samples.length) * 100);
        if (kind === "router") { root.routerPingMs = avgMs; root.routerLossPercent = lossPercent; }
        else { root.internetPingMs = avgMs; root.internetLossPercent = lossPercent; }
    }

    Timer {
        interval: 2000
        running: root.panelVisible
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            if (root.gatewayIp !== "") pingRouterProc.running = true;
            pingInternetProc.running = true;
        }
    }

    Process {
        id: pingRouterProc
        property string _buf: ""
        command: root.gatewayIp !== "" ? ["ping", "-n", "-c1", "-W1", root.gatewayIp] : ["true"]
        stdout: StdioCollector { onStreamFinished: pingRouterProc._buf = this.text }
        onExited: exitCode => {
            const m = /time=([\d.]+)/.exec(pingRouterProc._buf);
            root._recordPing("router", exitCode === 0 && m !== null, m ? parseFloat(m[1]) : 0);
        }
    }

    Process {
        id: pingInternetProc
        property string _buf: ""
        command: ["ping", "-n", "-c1", "-W1", "1.1.1.1"]
        stdout: StdioCollector { onStreamFinished: pingInternetProc._buf = this.text }
        onExited: exitCode => {
            const m = /time=([\d.]+)/.exec(pingInternetProc._buf);
            root._recordPing("internet", exitCode === 0 && m !== null, m ? parseFloat(m[1]) : 0);
        }
    }

    // Live rx/tx rate + cumulative totals, from sysfs interface counters
    // (reset on interface up, not lifetime) — panel-open-only, same as the
    // ping timer above. Supersedes NetSpeed.qml's pre-formatted string for
    // this panel only; the bar's NetSpeedWidget.qml is untouched.
    property real rxBytesTotal: 0
    property real txBytesTotal: 0
    property real rxBytesPerSec: 0
    property real txBytesPerSec: 0
    property var _lastNetSample: null // {rx, tx, t}

    Timer {
        interval: 1500
        running: root.panelVisible
        repeat: true
        triggeredOnStart: true
        onTriggered: if (root.ifaceName !== "") netStatsProc.running = true
    }

    Process {
        id: netStatsProc
        command: root.ifaceName !== ""
            ? ["sh", "-c", `cat /sys/class/net/${root.ifaceName}/statistics/rx_bytes /sys/class/net/${root.ifaceName}/statistics/tx_bytes 2>/dev/null`]
            : ["true"]
        stdout: StdioCollector {
            onStreamFinished: {
                const [rx, tx] = this.text.trim().split("\n").map(Number);
                if (isNaN(rx) || isNaN(tx)) return;
                const now = Date.now();
                const prev = root._lastNetSample;
                if (prev && now > prev.t) {
                    const dt = (now - prev.t) / 1000;
                    root.rxBytesPerSec = Math.max(0, (rx - prev.rx) / dt);
                    root.txBytesPerSec = Math.max(0, (tx - prev.tx) / dt);
                }
                root._lastNetSample = { rx, tx, t: now };
                root.rxBytesTotal = rx;
                root.txBytesTotal = tx;
            }
        }
    }

    property bool speedtestRunning: false
    property var speedtestResult: null
    // Distinct from speedtestResult === null so the panel can tell "never
    // run yet" (both null, both false) apart from "just ran and failed"
    // (result stays null, but this flips true) — previously a non-zero
    // exit or unparseable output silently reset speedtestRunning with no
    // other state change, so a failed test looked identical to one that
    // was simply never started.
    property bool speedtestFailed: false

    function runSpeedtest() {
        if (speedtestRunning) return;
        speedtestRunning = true;
        speedtestResult = null;
        speedtestFailed = false;
        speedtestProc.buffer = [];
        speedtestProc.running = true;
    }

    Process {
        id: speedtestProc
        property list<string> buffer: []
        command: ["speedtest-cli", "--simple"]
        stdout: SplitParser {
            onRead: line => speedtestProc.buffer.push(line)
        }
        onExited: exitCode => {
            root.speedtestRunning = false;
            if (exitCode !== 0) {
                root.speedtestFailed = true;
                return;
            }
            const text = speedtestProc.buffer.join("\n");
            const down = /Download:\s*([\d.]+)\s*Mbit\/s/.exec(text);
            const up = /Upload:\s*([\d.]+)\s*Mbit\/s/.exec(text);
            if (down && up) {
                root.speedtestResult = { down: parseFloat(down[1]), up: parseFloat(up[1]) };
            } else {
                root.speedtestFailed = true;
            }
        }
    }

    // Memoized like Cliphist.qml's thumbCache: once an ssid's QR image is
    // generated, re-showing it (e.g. reopening the panel) is a map lookup,
    // not a new `nmcli`+`qrencode` subprocess.
    property var qrCache: ({})
    property var _qrPending: ({})

    function _qrSanitize(ssid) {
        return ssid.replace(/[^A-Za-z0-9_-]/g, "_");
    }

    function wifiQrPath(ssid) {
        if (ssid === "") return "";
        if (root.qrCache[ssid]) return root.qrCache[ssid];
        if (root._qrPending[ssid]) return "";
        root._qrPending[ssid] = true;

        const secured = activeWifiNetwork?.security !== undefined
            && activeWifiNetwork?.security !== QN.WifiSecurityType.Open;
        const outPath = Quickshell.env("XDG_RUNTIME_DIR") + "/wifi-qr-" + root._qrSanitize(ssid) + ".png";
        const qSsid = root._shQuote(ssid);
        const qOut = root._shQuote(outPath);

        // Fetch the PSK first (secured networks only), then build the
        // WIFI: payload and hand it to qrencode. `nmcli -s` prints the
        // secret; an open network skips straight to the nopass payload.
        // ssid is bound to a shell variable via the same single-quote
        // escaping as everywhere else, then only ever expanded inside
        // double quotes ("$ssid") or via printf %s — never spliced into a
        // literal the shell re-parses — so SSID/PSK metacharacters can't
        // break out of the command.
        const cmd = secured
            ? "ssid=" + qSsid + "; psk=$(nmcli -s -g 802-11-wireless-security.psk connection show \"$ssid\") && "
                + "qrencode -o " + qOut + " \"$(printf 'WIFI:T:WPA;S:%s;P:%s;;' \"$ssid\" \"$psk\")\""
            : "ssid=" + qSsid + "; qrencode -o " + qOut + " \"$(printf 'WIFI:T:nopass;S:%s;;' \"$ssid\")\"";

        const proc = qrProcComponent.createObject(root, { command: ["sh", "-c", cmd] });
        proc.exited.connect(exitCode => {
            delete root._qrPending[ssid];
            if (exitCode === 0) {
                const cache = Object.assign({}, root.qrCache);
                cache[ssid] = outPath;
                root.qrCache = cache;
            }
            proc.destroy();
        });
        proc.running = true;
        return "";
    }

    Component {
        id: qrProcComponent
        Process {}
    }

    // Visibility for NetworkPanel.qml (modules/network/), same
    // toggle-flag-on-the-singleton pattern as Notifications.qml's
    // controlCenterVisible. panelScreenName records which monitor's
    // NetworkWidget was clicked, threaded through by the widget's own
    // Screen.name (see TooltipBus.qml for why this is mandatory: without
    // it every monitor's Bar reacts to the same shared flag and each
    // would pop its own copy of the panel).
    property bool panelVisible: false
    property string panelScreenName: ""

    function togglePanel(screenName) {
        if (panelVisible && panelScreenName === screenName) {
            panelVisible = false;
        } else {
            // All top-right panels anchor to the same bar position, so
            // leaving another one open makes it look like this click just
            // swapped its content instead of opening a new popup.
            Bluetooth.hidePanel();
            Battery.hidePanel();
            Brightness.hidePanel();
            Audio.hidePanel();
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
