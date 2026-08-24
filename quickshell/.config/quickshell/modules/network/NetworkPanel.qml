import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Networking as QN
import qs.theme
import qs.config
import qs.services

// Native WiFi management panel, opened by left-clicking NetworkWidget.qml
// (previously launched nm-applet). API surface confirmed against
// https://quickshell.org/docs/v0.3.0/types/Quickshell.Networking/ — Network
// devices/networks list live, WifiNetwork.connectWithPsk()/Network.connect()/
// .disconnect()/.forget() and Network.connectionFailed(reason) are all
// first-party (also cross-checked against basecamp/omarchy's own
// Panel.qml at shell/plugins/panels/network/Panel.qml, which uses the
// identical connect() → on NoSecrets failure → connectWithPsk() flow this
// panel follows, and confirms scannerEnabled is writable — see the
// onIsActiveChanged handler below. Enterprise/802.1x auth and the ping-based
// connection-quality readout are deliberately not ported — this is a bar
// status panel, not a full network manager replacement. DNS switching was
// tried and reverted: it broke the user's actual network).
Scope {
    id: root

    // One Bar instance per monitor shares this same panel component, so
    // gating on the clicked widget's own screen name is mandatory — see
    // TooltipBus.qml's cross-monitor writeup for the bug this avoids.
    required property string screenName
    readonly property bool isActive: Network.panelVisible && Network.panelScreenName === screenName

    readonly property var wifiDevice: QN.Networking.devices.values.find(d => d.type === QN.DeviceType.Wifi) ?? null
    readonly property var wiredDevice: QN.Networking.devices.values.find(d => d.type === QN.DeviceType.Wired) ?? null
    readonly property var wifiNetworks: {
        const list = wifiDevice?.networks.values ?? [];
        return [...list].sort((a, b) => {
            if (a.connected !== b.connected) return a.connected ? -1 : 1;
            if (a.known !== b.known) return a.known ? -1 : 1;
            return b.signalStrength - a.signalStrength;
        });
    }

    // A connected network never gets a section header (it's already obvious
    // which one it is); everything else is grouped as "KNOWN"/"OTHER". Only
    // label a group when both groups are actually present — a single-group
    // list doesn't need to say so.
    function wifiSectionTitle(index) {
        const list = root.wifiNetworks;
        if (index < 0 || index >= list.length) return "";
        const net = list[index];
        if (!net || net.connected) return "";

        const hasKnown = list.some(n => !n.connected && n.known);
        const hasOther = list.some(n => !n.connected && !n.known);
        if (!hasKnown || !hasOther) return "";

        const prev = index > 0 ? list[index - 1] : null;
        if (net.known && (!prev || prev.connected)) return "KNOWN NETWORKS";
        if (!net.known && (!prev || prev.connected || prev.known)) return "OTHER NETWORKS";
        return "";
    }

    property string passwordSsid: ""
    property string passwordText: ""
    property string actionSsid: ""
    property string failureSsid: ""
    property bool qrOpen: false

    // Quickshell.Networking's WifiDevice.scannerEnabled is writable (not
    // readonly as the docs page's terse property list implies at a glance —
    // confirmed by reading basecamp/omarchy's own Panel.qml, which drives
    // Wi-Fi scanning this exact way: shell/plugins/panels/network/Panel.qml's
    // setScannerEnabled()). Flipping it true kicks NetworkManager's scanner
    // and the live `networks` model repopulates itself over DBus — no nmcli
    // subprocess needed. On while the panel's open, off the moment it closes
    // (a bar widget/panel is instantiated once per monitor and shares the
    // same underlying device, so leaving the scanner claimed on close would
    // leak it across panel instances).
    onIsActiveChanged: {
        if (root.wifiDevice) root.wifiDevice.scannerEnabled = root.isActive;
    }
    Component.onDestruction: {
        if (root.wifiDevice) root.wifiDevice.scannerEnabled = false;
    }

    function networkForSsid(ssid) {
        return wifiNetworks.find(n => n.name === ssid) ?? null;
    }

    function tryConnect(network) {
        if (!network) return;
        failureSsid = "";
        actionSsid = network.name;
        network.connect();
    }

    function submitPassword(ssid) {
        const network = networkForSsid(ssid);
        if (!network || passwordText === "") return;
        failureSsid = "";
        actionSsid = ssid;
        network.connectWithPsk(passwordText);
        passwordSsid = "";
        passwordText = "";
    }

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: 340
            implicitHeight: Math.min(640, content.implicitHeight + 28)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                // Dismisses the panel on a click anywhere outside it — same
                // mechanism as BluetoothPanel.qml/ControlCenter.qml. Windows
                // must be non-empty before the grab activates, so gate on
                // root.isActive (true) rather than binding straight to true.
                active: root.isActive
                windows: [win]
                onCleared: Network.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                // PanelWindow's layershell surface isn't a QtQuick Item, so
                // Keys can't attach there directly (same pattern as
                // ThemePicker.qml's keyCatcher) — an Item with focus: true
                // is required to actually receive routed key events.
                focus: true

                Keys.onEscapePressed: Network.hidePanel()

                MouseArea {
                    // Eats clicks that land inside this window but outside
                    // any child control, so they don't fall through to
                    // whatever's behind the panel.
                    anchors.fill: parent
                    z: -1
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    // Small pill button used by the band row below.
                    component Chip: Rectangle {
                        property string label
                        signal activated()
                        Layout.fillWidth: true
                        implicitHeight: 24
                        radius: 6
                        color: Theme.surface1

                        Text {
                            anchors.centerIn: parent
                            text: parent.label
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: 11
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: parent.activated()
                        }
                    }

                    // Small icon button used by the header's QR/speed-test
                    // triggers below.
                    component IconButton: Rectangle {
                        property string glyph
                        property color tint: Theme.text
                        signal activated()
                        implicitWidth: 26
                        implicitHeight: 26
                        radius: 6
                        color: Theme.surface1

                        Text {
                            anchors.centerIn: parent
                            text: parent.glyph
                            color: parent.tint
                            font.family: Config.fontFamily
                            font.pixelSize: 13
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: parent.activated()
                        }
                    }

                    // Hero row: icon + SSID/status + QR/speedtest icon
                    // buttons + Wi-Fi radio toggle.
                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            text: root.wiredDevice?.connected ? "\u{F0200}" : "\u{F05A9}"
                            color: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: 22
                        }

                        ColumnLayout {
                            spacing: 0
                            Layout.fillWidth: true

                            Text {
                                text: root.wiredDevice?.connected ? "Ethernet" : (Network.ssid !== "" ? Network.ssid : "Not Connected")
                                color: Theme.text
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: 15
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                            }
                            Text {
                                text: (root.wiredDevice?.connected ? "WIRED" : "WI-FI") + (Network.vpnActive ? " · VPN" : "")
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: 10
                            }
                        }

                        IconButton {
                            visible: Network.kind === "wifi" && Network.ssid !== ""
                            glyph: "\u{F0432}" // nf-md-qrcode
                            onActivated: {
                                root.qrOpen = !root.qrOpen;
                                if (root.qrOpen) Network.wifiQrPath(Network.ssid);
                            }
                        }

                        IconButton {
                            visible: Network.kind !== "disconnected"
                            glyph: "\u{F0489}" // nf-md-speedometer (speedtest trigger)
                            tint: Network.speedtestRunning ? Theme.blue : Theme.text
                            onActivated: Network.runSpeedtest()
                        }

                        // Wi-Fi radio toggle, same visual as ControlCenter.qml's DND toggle.
                        Rectangle {
                            width: 36
                            height: 20
                            radius: 10
                            color: QN.Networking.wifiEnabled ? Theme.blue : Theme.surface2

                            Rectangle {
                                width: 16
                                height: 16
                                radius: 8
                                color: Theme.text
                                anchors.verticalCenter: parent.verticalCenter
                                x: QN.Networking.wifiEnabled ? parent.width - width - 2 : 2
                                Behavior on x { NumberAnimation { duration: 120 } }
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: QN.Networking.wifiEnabled = !QN.Networking.wifiEnabled
                            }
                        }
                    }

                    // QR reveal (from the header's QR icon button).
                    ColumnLayout {
                        visible: root.qrOpen && Network.kind === "wifi"
                        Layout.fillWidth: true
                        Layout.alignment: Qt.AlignHCenter
                        spacing: 4

                        Text {
                            visible: !Network.qrCache[Network.ssid]
                            text: "Generating…"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 11
                            Layout.alignment: Qt.AlignHCenter
                        }

                        Image {
                            visible: !!Network.qrCache[Network.ssid]
                            source: Network.qrCache[Network.ssid] ? "file://" + Network.qrCache[Network.ssid] : ""
                            Layout.preferredWidth: 160
                            Layout.preferredHeight: 160
                            Layout.alignment: Qt.AlignHCenter
                            fillMode: Image.PreserveAspectFit
                            cache: false
                        }
                    }

                    // Stat grid: Ping/Packet Loss, Receiving/Sending,
                    // Downloaded/Uploaded, IP Address/Gateway.
                    GridLayout {
                        visible: Network.kind !== "disconnected"
                        Layout.fillWidth: true
                        columns: 2
                        rowSpacing: 4
                        columnSpacing: 12

                        component StatText: RowLayout {
                            Layout.fillWidth: true
                            property string label
                            property string value
                            Text { text: parent.label; color: Theme.subtext0; font.family: Config.fontFamily; font.pixelSize: 11; Layout.fillWidth: true }
                            Text { text: parent.value; color: Theme.text; font.family: Config.fontFamily; font.bold: true; font.pixelSize: 11 }
                        }

                        function fmtMs(ms) { return ms < 0 ? "—" : Math.round(ms) + " ms"; }
                        function fmtRate(bps) {
                            const mb = bps / 1048576;
                            return mb >= 1 ? mb.toFixed(1) + " MB/s" : (bps / 1024).toFixed(0) + " KB/s";
                        }
                        function fmtBytes(b) {
                            const gb = b / 1073741824;
                            if (gb >= 1) return gb.toFixed(2) + " GB";
                            return (b / 1048576).toFixed(0) + " MB";
                        }

                        StatText { label: "Ping"; value: parent.fmtMs(Network.internetPingMs) }
                        StatText { label: "Packet Loss"; value: Network.internetLossPercent + "%" }
                        StatText { label: "Receiving"; value: parent.fmtRate(Network.rxBytesPerSec) }
                        StatText { label: "Sending"; value: parent.fmtRate(Network.txBytesPerSec) }
                        StatText { label: "Downloaded"; value: parent.fmtBytes(Network.rxBytesTotal) }
                        StatText { label: "Uploaded"; value: parent.fmtBytes(Network.txBytesTotal) }
                        StatText { label: "IP Address"; value: Network.localIp !== "" ? Network.localIp : "—" }
                        StatText { label: "Gateway"; value: Network.gatewayIp !== "" ? Network.gatewayIp : "—" }
                    }

                    // WI-FI BAND: current band + Automatic toggle (off
                    // reveals the existing band-pin chips below).
                    RowLayout {
                        visible: Network.kind === "wifi"
                        Layout.fillWidth: true
                        Layout.topMargin: 2
                        spacing: 6

                        Text {
                            text: "WI-FI BAND: " + (Network.currentBandLabel !== "" ? Network.currentBandLabel : "—")
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 10
                            font.bold: true
                            Layout.fillWidth: true
                        }

                        Text {
                            text: "AUTOMATIC"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 9
                        }

                        Rectangle {
                            width: 32
                            height: 18
                            radius: 9
                            color: Network.bandAutomatic ? Theme.blue : Theme.surface2

                            Rectangle {
                                width: 14
                                height: 14
                                radius: 7
                                color: Theme.text
                                anchors.verticalCenter: parent.verticalCenter
                                x: Network.bandAutomatic ? parent.width - width - 2 : 2
                                Behavior on x { NumberAnimation { duration: 120 } }
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: Network.bandAutomatic ? (Network.bandAutomatic = false) : Network.setBand("auto")
                            }
                        }
                    }

                    RowLayout {
                        visible: Network.kind === "wifi" && !Network.bandAutomatic
                        Layout.fillWidth: true
                        spacing: 6

                        Chip { label: "2.4GHz"; onActivated: Network.setBand("bg") }
                        Chip { label: "5GHz"; onActivated: Network.setBand("a") }
                    }

                    Rectangle { Layout.fillWidth: true; height: 1; color: Theme.surface2 }

                    Flickable {
                        Layout.fillWidth: true
                        // See BluetoothPanel.qml's identical fix: fillHeight
                        // doesn't feed back into the parent ColumnLayout's
                        // implicitHeight, which PanelWindow relies on to
                        // size itself, so the window collapses to just the
                        // header without an explicit preferredHeight here.
                        Layout.preferredHeight: Math.min(260, list.implicitHeight)
                        contentHeight: list.implicitHeight
                        clip: true
                        visible: QN.Networking.wifiEnabled

                        ColumnLayout {
                            id: list
                            width: parent.width
                            spacing: 4

                            Text {
                                visible: root.wifiNetworks.length === 0
                                text: root.wifiDevice?.scannerEnabled ? "Scanning…" : "No networks found"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: 12
                                Layout.topMargin: 20
                                Layout.alignment: Qt.AlignHCenter
                            }

                            Repeater {
                                model: root.wifiNetworks
                                delegate: ColumnLayout {
                                    id: row
                                    required property var modelData
                                    required property int index
                                    Layout.fillWidth: true
                                    spacing: 4

                                    readonly property bool isPasswordOpen: root.passwordSsid === modelData.name
                                    readonly property bool isBusy: root.actionSsid === modelData.name && modelData.stateChanging
                                    readonly property bool isFailed: root.failureSsid === modelData.name
                                    readonly property bool secured: modelData.security !== QN.WifiSecurityType.Open
                                    readonly property string sectionTitle: root.wifiSectionTitle(index)

                                    Text {
                                        visible: row.sectionTitle !== ""
                                        text: row.sectionTitle
                                        color: Theme.overlay0
                                        font.family: Config.fontFamily
                                        font.bold: true
                                        font.pixelSize: 10
                                        Layout.topMargin: 6
                                        Layout.leftMargin: 8
                                    }

                                    Rectangle {
                                        Layout.fillWidth: true
                                        implicitHeight: rowContent.implicitHeight + 16
                                        radius: 8
                                        color: modelData.connected ? Theme.surface1 : "transparent"

                                        RowLayout {
                                            id: rowContent
                                            anchors.fill: parent
                                            anchors.margins: 8
                                            spacing: 8

                                            Text {
                                                text: {
                                                    const s = modelData.signalStrength;
                                                    if (s > 0.8) return "\u{F0928}";
                                                    if (s > 0.6) return "\u{F0925}";
                                                    if (s > 0.4) return "\u{F0922}";
                                                    if (s > 0.2) return "\u{F091F}";
                                                    return "\u{F092F}";
                                                }
                                                color: Theme.text
                                                font.family: Config.fontFamily
                                                font.pixelSize: 14
                                            }

                                            Text {
                                                text: modelData.name
                                                color: Theme.text
                                                font.family: Config.fontFamily
                                                font.pixelSize: 12
                                                Layout.fillWidth: true
                                                elide: Text.ElideRight
                                            }

                                            Text {
                                                visible: row.secured
                                                text: "\u{F0BC7}"
                                                color: Theme.overlay0
                                                font.family: Config.fontFamily
                                                font.pixelSize: 11
                                            }

                                            Text {
                                                visible: modelData.connected
                                                text: "\u{F012C}"
                                                color: Theme.green
                                                font.family: Config.fontFamily
                                                font.pixelSize: 13
                                            }

                                            Text {
                                                visible: row.isBusy
                                                text: "…"
                                                color: Theme.subtext0
                                                font.family: Config.fontFamily
                                                font.pixelSize: 12
                                            }
                                        }

                                        MouseArea {
                                            anchors.fill: parent
                                            enabled: !row.isBusy
                                            onClicked: {
                                                if (modelData.connected) {
                                                    modelData.disconnect();
                                                } else if (row.secured && !modelData.known) {
                                                    root.passwordSsid = modelData.name;
                                                    root.passwordText = "";
                                                } else {
                                                    root.tryConnect(modelData);
                                                }
                                            }
                                        }
                                    }

                                    RowLayout {
                                        visible: row.isPasswordOpen
                                        Layout.fillWidth: true
                                        Layout.leftMargin: 8
                                        Layout.rightMargin: 8
                                        spacing: 6

                                        Rectangle {
                                            Layout.fillWidth: true
                                            implicitHeight: 28
                                            radius: 6
                                            color: Theme.crust
                                            border.color: Theme.surface2
                                            border.width: 1

                                            TextInput {
                                                id: pwInput
                                                anchors.fill: parent
                                                anchors.margins: 6
                                                color: Theme.text
                                                font.family: Config.fontFamily
                                                font.pixelSize: 12
                                                echoMode: TextInput.Password
                                                clip: true
                                                focus: row.isPasswordOpen
                                                text: root.passwordText
                                                onTextChanged: root.passwordText = text
                                                Keys.onReturnPressed: root.submitPassword(row.modelData.name)
                                                Keys.onEscapePressed: { root.passwordSsid = ""; root.passwordText = ""; }
                                            }
                                        }

                                        Rectangle {
                                            implicitWidth: 50
                                            implicitHeight: 28
                                            radius: 6
                                            color: Theme.blue

                                            Text {
                                                anchors.centerIn: parent
                                                text: "Join"
                                                color: Theme.crust
                                                font.family: Config.fontFamily
                                                font.pixelSize: 11
                                                font.bold: true
                                            }

                                            MouseArea {
                                                anchors.fill: parent
                                                onClicked: root.submitPassword(row.modelData.name)
                                            }
                                        }
                                    }

                                    Text {
                                        visible: row.isFailed
                                        text: "Failed to connect — check the password"
                                        color: Theme.red
                                        font.family: Config.fontFamily
                                        font.pixelSize: 11
                                        Layout.leftMargin: 8
                                    }

                                    Connections {
                                        target: row.modelData
                                        function onConnectionFailed(reason) {
                                            if (root.actionSsid !== row.modelData.name) return;
                                            root.actionSsid = "";
                                            root.failureSsid = row.modelData.name;
                                            if (reason === QN.ConnectionFailReason.NoSecrets) {
                                                root.passwordSsid = row.modelData.name;
                                            }
                                        }
                                        function onConnectedChanged() {
                                            if (row.modelData.connected && root.actionSsid === row.modelData.name) {
                                                root.actionSsid = "";
                                                root.failureSsid = "";
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // --- Speed test result (from the header's speedtest icon button) ---
                    Text {
                        visible: Network.speedtestResult !== null
                        text: Network.speedtestResult
                            ? ("\u{F01DA} " + Network.speedtestResult.down.toFixed(1) + " ↓  " + Network.speedtestResult.up.toFixed(1) + " ↑ Mbps")
                            : ""
                        color: Theme.subtext0
                        font.family: Config.fontFamily
                        font.pixelSize: 11
                        Layout.fillWidth: true
                        elide: Text.ElideRight
                    }
                }
            }
        }
    }
}
