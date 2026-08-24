import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.theme
import qs.config
import qs.services

// Replaces hyprpolkitagent's dialog. Backed by services/Polkit.qml's
// PolkitAgent; styled to match services/Lock.qml's LockScreen input field.
// AuthenticationFlow's message/inputPrompt/isResponseRequired/submit()/
// cancelAuthenticationRequest() surface confirmed via
// quickshell.org/docs/types/Quickshell.Services.Polkit/PolkitAgent and
// basecamp/omarchy's shell/plugins/polkit/PolkitAgent.qml (same module).
Scope {
    id: root

    readonly property bool dialogVisible: Polkit.dialogVisible
    property bool submitted: false
    property bool failed: false

    function currentFlow() {
        return Polkit.flow;
    }

    function submitResponse() {
        const flow = currentFlow();
        if (!flow || !flow.isResponseRequired) return;
        root.submitted = true;
        flow.submit(field.text);
        field.text = "";
    }

    function cancel() {
        const flow = currentFlow();
        field.text = "";
        root.submitted = false;
        if (flow) flow.cancelAuthenticationRequest();
    }

    Connections {
        target: Polkit.flow

        function onAuthenticationFailed() {
            root.submitted = false;
            root.failed = true;
            failedTimer.restart();
        }

        function onAuthenticationSucceeded() {
            root.failed = false;
        }
    }

    LazyLoader {
        active: root.dialogVisible

        PanelWindow {
            id: win
            anchors { top: true; bottom: true; left: true; right: true }
            color: "transparent"
            WlrLayershell.namespace: "quickshell-polkit"
            WlrLayershell.layer: WlrLayer.Overlay
            WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
            exclusionMode: ExclusionMode.Ignore

            Rectangle {
                anchors.fill: parent
                color: Qt.rgba(Theme.crust.r, Theme.crust.g, Theme.crust.b, 0.55)
            }

            Rectangle {
                id: card
                width: 360
                height: content.implicitHeight + 32
                radius: 14
                anchors.centerIn: parent
                color: Theme.surface0
                border.color: root.failed ? Theme.red : Theme.surface2
                border.width: 1

                Keys.onEscapePressed: root.cancel()
                focus: true

                MouseArea { anchors.fill: parent; z: -1 }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 10

                    Text {
                        Layout.fillWidth: true
                        text: Polkit.flow?.message ?? "Authentication is required"
                        color: Theme.text
                        wrapMode: Text.WordWrap
                        font.family: Config.fontFamily
                        font.pixelSize: 14
                        font.bold: true
                    }

                    Text {
                        Layout.fillWidth: true
                        visible: text.length > 0
                        text: Polkit.flow?.inputPrompt ?? ""
                        color: Theme.subtext0
                        wrapMode: Text.WordWrap
                        font.family: Config.fontFamily
                        font.pixelSize: 12
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: 42
                        radius: 8
                        color: Theme.surface1
                        border.color: root.failed ? Theme.red : (field.activeFocus ? Theme.blue : "transparent")
                        border.width: 1

                        TextInput {
                            id: field
                            anchors.fill: parent
                            anchors.leftMargin: 10
                            anchors.rightMargin: 10
                            verticalAlignment: TextInput.AlignVCenter
                            echoMode: (Polkit.flow?.responseVisible ?? false) ? TextInput.Normal : TextInput.Password
                            passwordCharacter: "●"
                            enabled: !root.submitted
                            readOnly: root.submitted
                            color: Theme.text
                            selectionColor: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: 14
                            focus: true

                            Component.onCompleted: forceActiveFocus()
                            onAccepted: root.submitResponse()
                            Keys.onEscapePressed: root.cancel()
                        }

                        Text {
                            anchors.fill: field
                            visible: field.text.length === 0
                            text: root.submitted ? "Checking…" : "Enter password"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 13
                        }
                    }

                    Text {
                        Layout.fillWidth: true
                        visible: root.failed
                        text: "Authentication failed, try again"
                        color: Theme.red
                        font.family: Config.fontFamily
                        font.pixelSize: 11
                    }
                }
            }
        }
    }

    Timer {
        id: failedTimer
        interval: 1500
        onTriggered: root.failed = false
    }
}
