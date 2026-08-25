import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Bluetooth as QB
import qs.theme
import qs.config
import qs.services

// Native Bluetooth management panel, opened by left-clicking
// BluetoothWidget.qml (previously launched blueman-manager). API surface
// confirmed against https://quickshell.org/docs/v0.3.0/types/Quickshell.Bluetooth/
// — BluetoothAdapter.enabled/discovering are read-write (cross-checked
// against real usage in snowarch/iNiR and corecathx/whisker, since the
// doc fetch tool's own property-readonly/readwrite categorization proved
// unreliable), and BluetoothDevice.pair()/connect()/disconnect()/forget()
// are all first-party methods — no bluetoothctl involved.
//
// Ported additions from basecamp/omarchy's shell/plugins/panels/bluetooth:
// - Connected/Paired/Available sections instead of one flat list.
// - Filters out devices whose only "name" is a MAC address or UUID (noisy
//   BLE advertisers), via isUuidLike/isAddressLike/hasHumanName below.
// - Auto-managed discovery: scans while the panel is open, stops cleanly
//   once it closes, instead of a manual Scan/Stop button. Bar.qml
//   instantiates one BluetoothPanel per screen, but only one can ever have
//   isActive true at a time (Bluetooth.panelVisible/panelScreenName is a
//   single shared singleton, see Bluetooth.qml), so unlike Omarchy's
//   multi-widget-per-bar setup there's no cross-instance discovery-debt
//   handoff needed here — the one instance that opened it is always the
//   one that's still around to stop it.
// - Automatic default-audio-output switch when a device finishes
//   connecting, matching it to a Pipewire sink by address/name. Reuses
//   Audio.qml's already-tracked `outputDevices` (bound via its
//   PwObjectTracker) instead of adding a separate Pipewire binding here.
// - Richer status text (Connecting…/Disconnecting…/Pairing…) driven by
//   BluetoothDeviceState instead of just the `pairing` flag.
// - j/k (or arrow) + Enter/x keyboard navigation. Not present in any other
//   panel in this repo, but requested specifically for this one.
Scope {
    id: root

    // Same cross-monitor gating as NetworkPanel.qml/TooltipBus.qml.
    required property string screenName
    readonly property bool isActive: Bluetooth.panelVisible && Bluetooth.panelScreenName === screenName

    readonly property var adapter: QB.Bluetooth.defaultAdapter ?? null
    readonly property var rawDevices: adapter?.devices.values ?? []

    function deviceLabel(d) {
        return String(d?.name || d?.deviceName || "").trim();
    }

    function isUuidLike(value) {
        const text = String(value ?? "").trim();
        if (text === "") return false;
        return /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(text)
            || /^[0-9a-f]{32}$/i.test(text)
            || /^0x[0-9a-f]{4,32}$/i.test(text);
    }

    function isAddressLike(value) {
        return /^([0-9a-f]{2}[:-]){5}[0-9a-f]{2}$/i.test(String(value ?? "").trim());
    }

    function hasHumanName(d) {
        const label = deviceLabel(d);
        return label !== "" && !isUuidLike(label) && !isAddressLike(label);
    }

    function sortedByLabel(list) {
        return [...list].sort((a, b) => deviceLabel(a).localeCompare(deviceLabel(b)));
    }

    readonly property var namedDevices: rawDevices.filter(d => d && hasHumanName(d))
    readonly property var connectedDevices: sortedByLabel(namedDevices.filter(d => d.connected))
    readonly property var knownDevices: sortedByLabel(namedDevices.filter(d => !d.connected && (d.paired || d.bonded || d.trusted)))
    readonly property var discoveredDevices: sortedByLabel(namedDevices.filter(d => !d.connected && !d.paired && !d.bonded && !d.trusted))

    readonly property var visibleSections: {
        const s = [];
        if (connectedDevices.length > 0) s.push("connected");
        if (knownDevices.length > 0) s.push("known");
        if ((adapter?.discovering ?? false) && discoveredDevices.length > 0) s.push("discovered");
        return s;
    }

    function devicesForSection(section) {
        if (section === "connected") return connectedDevices;
        if (section === "known") return knownDevices;
        if (section === "discovered") return discoveredDevices;
        return [];
    }

    function sectionCount(section) { return devicesForSection(section).length; }

    // ---- Keyboard-driven cursor: "header" is the virtual section for the
    // adapter on/off switch, sitting above the device sections so it stays
    // reachable by keyboard even with no devices listed.
    property string focusSection: "header"
    property int selectedIndex: 0
    property bool cursorActive: false

    function clampCursor() {
        const sections = visibleSections;
        if (focusSection === "header") return;
        if (sections.indexOf(focusSection) < 0) { focusSection = sections[0] ?? "header"; selectedIndex = 0; return; }
        const count = sectionCount(focusSection);
        if (count === 0) {
            const i = sections.indexOf(focusSection);
            focusSection = i > 0 ? sections[i - 1] : "header";
            selectedIndex = Math.max(0, sectionCount(focusSection) - 1);
            return;
        }
        if (selectedIndex > count - 1) selectedIndex = count - 1;
        if (selectedIndex < 0) selectedIndex = 0;
    }
    onVisibleSectionsChanged: clampCursor()

    function moveCursor(delta) {
        cursorActive = true;
        const sections = visibleSections;
        if (focusSection === "header") {
            if (delta > 0 && sections.length > 0) { focusSection = sections[0]; selectedIndex = 0; }
            return;
        }
        if (sections.length === 0) { focusSection = "header"; return; }
        const sIdx = sections.indexOf(focusSection);
        const idx = selectedIndex;
        const max = sectionCount(focusSection) - 1;
        if (delta > 0) {
            if (idx < max) { selectedIndex = idx + 1; return; }
            if (sIdx < sections.length - 1) { focusSection = sections[sIdx + 1]; selectedIndex = 0; }
        } else {
            if (idx > 0) { selectedIndex = idx - 1; return; }
            if (sIdx > 0) { focusSection = sections[sIdx - 1]; selectedIndex = sectionCount(focusSection) - 1; }
            else { focusSection = "header"; }
        }
    }

    function selectedDevice() {
        return devicesForSection(focusSection)[selectedIndex] ?? null;
    }

    function activateCursor() {
        if (focusSection === "header") { if (adapter) adapter.enabled = !adapter.enabled; return; }
        const dev = selectedDevice();
        if (!dev) return;
        if (dev.connected) dev.disconnect();
        else if (dev.paired || dev.bonded || dev.trusted) dev.connect();
        else dev.pair();
    }

    function forgetSelected() {
        if (focusSection === "header") return;
        const dev = selectedDevice();
        if (dev) dev.forget();
    }

    // ---- Auto-managed discovery. BlueZ's discovery session is held by this
    // quickshell D-Bus connection, so nothing ends it on its own — without
    // the stop timer below, opening this panel once would leave the radio
    // scanning until the next shell restart.
    property bool owesDiscoveryStop: false

    Connections {
        target: root.adapter
        function onDiscoveringChanged() {
            if (root.adapter && !root.adapter.discovering) root.owesDiscoveryStop = false;
        }
    }

    Timer {
        id: discoveryRetry
        interval: 1000
        repeat: true
        triggeredOnStart: true
        running: root.isActive && (root.adapter?.enabled ?? false) && !(root.adapter?.discovering ?? false)
        onTriggered: { root.owesDiscoveryStop = true; root.adapter.discovering = true; }
    }

    Timer {
        id: discoveryStop
        interval: 1000
        repeat: true
        property int attempts: 0
        running: !root.isActive && root.owesDiscoveryStop && (root.adapter?.discovering ?? false)
        onRunningChanged: if (running) attempts = 0
        onTriggered: {
            attempts += 1;
            if (attempts > 3) { root.owesDiscoveryStop = false; return; }
            root.adapter.discovering = false;
        }
    }

    // ---- Auto-switch default audio output on connect.
    property var _prevConnectedAddresses: []
    property string pendingAudioSwitchAddress: ""
    property int pendingAudioSwitchAttempts: 0

    function normalizedAddress(v) {
        return String(v ?? "").toLowerCase().replace(/[^0-9a-f]/g, "");
    }

    function bluetoothSinkForDevice(device) {
        if (!device) return null;
        const addr = normalizedAddress(device.address);
        const label = deviceLabel(device).toLowerCase();
        const sinks = Audio.outputDevices;
        for (const node of sinks) {
            const text = [node.name, node.description, node.nickname].filter(Boolean).join(" ").toLowerCase();
            if (addr !== "" && normalizedAddress(text).indexOf(addr) !== -1) return node;
            if (label !== "" && text.indexOf(label) !== -1) return node;
        }
        return null;
    }

    Timer {
        id: audioSwitchTimer
        interval: 500
        repeat: false
        onTriggered: root.tryAudioSwitch()
    }

    function tryAudioSwitch() {
        if (pendingAudioSwitchAddress === "") return;
        const dev = connectedDevices.find(d => d.address === pendingAudioSwitchAddress);
        if (!dev) { pendingAudioSwitchAddress = ""; return; }
        const sink = bluetoothSinkForDevice(dev);
        if (sink) { Audio.setOutputDevice(sink); pendingAudioSwitchAddress = ""; return; }
        pendingAudioSwitchAttempts += 1;
        if (pendingAudioSwitchAttempts >= 8) { pendingAudioSwitchAddress = ""; return; }
        audioSwitchTimer.restart();
    }

    onConnectedDevicesChanged: {
        const addrs = connectedDevices.map(d => d.address);
        const newOnes = addrs.filter(a => _prevConnectedAddresses.indexOf(a) === -1);
        if (newOnes.length > 0) {
            pendingAudioSwitchAddress = newOnes[0];
            pendingAudioSwitchAttempts = 0;
            audioSwitchTimer.restart();
        }
        _prevConnectedAddresses = addrs;
    }

    function statusTextFor(dev) {
        if (!dev) return "";
        if (dev.state === QB.BluetoothDeviceState.Connecting) return "Connecting…";
        if (dev.state === QB.BluetoothDeviceState.Disconnecting) return "Disconnecting…";
        if (dev.pairing) return "Pairing…";
        if (dev.connected && dev.batteryAvailable) return Math.round(dev.battery * 100) + "%";
        return "";
    }

    onIsActiveChanged: {
        if (isActive) {
            // Adopt a discovery session already running (e.g. left on from
            // an earlier open this same instance didn't get to stop yet).
            if (adapter?.discovering) owesDiscoveryStop = true;
            cursorActive = true;
            if (connectedDevices.length > 0) focusSection = "connected";
            else if (knownDevices.length > 0) focusSection = "known";
            else if (discoveredDevices.length > 0) focusSection = "discovered";
            else focusSection = "header";
            selectedIndex = 0;
        }
    }

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; right: true }
            margins { top: Config.barHeight + 4; right: 10 }
            implicitWidth: Config.px(340)
            implicitHeight: Math.min(520, content.implicitHeight + 28)
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            HyprlandFocusGrab {
                // Dismisses the panel on a click anywhere outside it — same
                // mechanism as NetworkPanel.qml/ControlCenter.qml.
                active: root.isActive
                windows: [win]
                onCleared: Bluetooth.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                // PanelWindow's layershell surface isn't a QtQuick Item, so
                // Keys can't attach there directly — an Item with
                // focus: true is required to receive routed key events (same
                // pattern as NetworkPanel.qml/ThemePicker.qml's keyCatcher).
                focus: true

                Keys.onEscapePressed: Bluetooth.hidePanel()
                Keys.onPressed: event => {
                    if (event.key === Qt.Key_J || event.key === Qt.Key_Down) { root.moveCursor(1); event.accepted = true; }
                    else if (event.key === Qt.Key_K || event.key === Qt.Key_Up) { root.moveCursor(-1); event.accepted = true; }
                    else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) { root.activateCursor(); event.accepted = true; }
                    else if (event.key === Qt.Key_X) { root.forgetSelected(); event.accepted = true; }
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    readonly property bool headerFocused: root.cursorActive && root.focusSection === "header"

                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "Bluetooth"
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(15)
                            Layout.fillWidth: true
                        }

                        Text {
                            visible: (root.adapter?.enabled ?? false) && (root.adapter?.discovering ?? false)
                            text: "Scanning…"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(10)
                        }

                        Rectangle {
                            width: 40
                            height: 22
                            radius: 11
                            color: (root.adapter?.enabled ?? false) ? Theme.blue : Theme.surface2
                            border.color: Theme.blue
                            border.width: content.headerFocused ? 2 : 0

                            Rectangle {
                                width: 18
                                height: 18
                                radius: 9
                                color: Theme.text
                                anchors.verticalCenter: parent.verticalCenter
                                x: (root.adapter?.enabled ?? false) ? parent.width - width - 2 - (content.headerFocused ? 2 : 0) : 2 + (content.headerFocused ? 2 : 0)
                                Behavior on x { NumberAnimation { duration: 120 } }
                            }

                            MouseArea {
                                anchors.fill: parent
                                enabled: root.adapter !== null
                                onClicked: root.adapter.enabled = !root.adapter.enabled
                            }

                            HoverHandler {
                                onHoveredChanged: if (hovered) { root.cursorActive = true; root.focusSection = "header"; }
                            }
                        }
                    }

                    Flickable {
                        Layout.fillWidth: true
                        // Layout.fillHeight doesn't feed back into the
                        // parent ColumnLayout's implicitHeight (which
                        // PanelWindow uses to size itself), so an
                        // explicit preferredHeight derived from the
                        // actual list content is required — otherwise
                        // the window collapses to just the header.
                        Layout.preferredHeight: Math.min(340, list.implicitHeight)
                        contentHeight: list.implicitHeight
                        clip: true
                        visible: root.adapter?.enabled ?? false

                        ColumnLayout {
                            id: list
                            width: parent.width
                            spacing: 10

                            Text {
                                visible: root.namedDevices.length === 0
                                text: (root.adapter?.discovering ?? false) ? "Scanning…" : "No devices"
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(12)
                                Layout.topMargin: 20
                                Layout.alignment: Qt.AlignHCenter
                            }

                            component SectionHeader: Text {
                                property string label
                                text: label
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.bold: true
                                font.pixelSize: Config.px(10)
                            }

                            component DeviceRow: Rectangle {
                                id: row
                                required property var dev
                                required property string sectionName
                                required property int rowIndex

                                readonly property bool selected: root.cursorActive && root.focusSection === sectionName && root.selectedIndex === rowIndex
                                readonly property string statusText: root.statusTextFor(dev)

                                Layout.fillWidth: true
                                implicitHeight: rowContent.implicitHeight + 16
                                radius: 8
                                color: (dev.connected || selected) ? Theme.surface1 : "transparent"
                                border.color: Theme.blue
                                border.width: selected ? 1 : 0

                                HoverHandler {
                                    onHoveredChanged: if (hovered) {
                                        root.cursorActive = true;
                                        root.focusSection = row.sectionName;
                                        root.selectedIndex = row.rowIndex;
                                    }
                                }

                                RowLayout {
                                    id: rowContent
                                    anchors.fill: parent
                                    anchors.margins: 8
                                    spacing: 8

                                    Text {
                                        text: "\u{F00E1}"
                                        color: Theme.text
                                        font.family: Config.fontFamily
                                        font.pixelSize: Config.px(14)
                                    }

                                    Text {
                                        text: row.dev.name !== "" ? row.dev.name : row.dev.deviceName
                                        color: Theme.text
                                        font.family: Config.fontFamily
                                        font.pixelSize: Config.px(12)
                                        Layout.fillWidth: true
                                        elide: Text.ElideRight
                                    }

                                    Text {
                                        visible: row.statusText !== ""
                                        text: row.statusText
                                        color: Theme.subtext0
                                        font.family: Config.fontFamily
                                        font.pixelSize: Config.px(11)
                                    }

                                    Rectangle {
                                        implicitWidth: actionLabel.implicitWidth + 16
                                        implicitHeight: Config.px(22)
                                        radius: 6
                                        color: row.dev.connected ? Theme.surface2 : Theme.blue

                                        Text {
                                            id: actionLabel
                                            anchors.centerIn: parent
                                            text: row.dev.connected ? "Disconnect" : (row.dev.paired ? "Connect" : "Pair")
                                            color: row.dev.connected ? Theme.text : Theme.crust
                                            font.family: Config.fontFamily
                                            font.pixelSize: Config.px(10)
                                            font.bold: true
                                        }

                                        MouseArea {
                                            anchors.fill: parent
                                            onClicked: {
                                                if (row.dev.connected) row.dev.disconnect();
                                                else if (row.dev.paired) row.dev.connect();
                                                else row.dev.pair();
                                            }
                                        }
                                    }

                                    Text {
                                        visible: row.dev.paired
                                        text: "\u{F0156}"
                                        color: Theme.overlay0
                                        font.family: Config.fontFamily
                                        font.pixelSize: Config.px(12)

                                        MouseArea {
                                            anchors.fill: parent
                                            onClicked: row.dev.forget()
                                        }
                                    }
                                }
                            }

                            // ---- Connected ----
                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 4
                                visible: root.connectedDevices.length > 0

                                SectionHeader { label: "CONNECTED" }

                                Repeater {
                                    model: root.connectedDevices
                                    delegate: DeviceRow {
                                        required property var modelData
                                        required property int index
                                        dev: modelData
                                        sectionName: "connected"
                                        rowIndex: index
                                    }
                                }
                            }

                            // ---- Paired ----
                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 4
                                visible: root.knownDevices.length > 0

                                SectionHeader { label: "PAIRED" }

                                Repeater {
                                    model: root.knownDevices
                                    delegate: DeviceRow {
                                        required property var modelData
                                        required property int index
                                        dev: modelData
                                        sectionName: "known"
                                        rowIndex: index
                                    }
                                }
                            }

                            // ---- Available (only while scanning) ----
                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 4
                                visible: (root.adapter?.discovering ?? false) && root.discoveredDevices.length > 0

                                SectionHeader { label: "AVAILABLE" }

                                Repeater {
                                    model: root.discoveredDevices
                                    delegate: DeviceRow {
                                        required property var modelData
                                        required property int index
                                        dev: modelData
                                        sectionName: "discovered"
                                        rowIndex: index
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
