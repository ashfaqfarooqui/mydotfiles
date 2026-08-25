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
    property int shakeOffset: 0

    function currentFlow() {
        return Polkit.flow;
    }

    // Reformats the raw D-Bus message ("Authentication is needed to run
    // `/usr/bin/foo` as the super user") into "Authorize running 'foo'".
    // Ported from basecamp/omarchy's PolkitModel.js authorizationLabel().
    function authorizationLabel(message) {
        const text = String(message || "");
        const match = text.match(/^Authentication is (?:needed|required) to run [`']([^`']+)[`'] as /i);
        return match ? "Authorize running '" + match[1] + "'" : text;
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
            shakeAnimation.restart();
        }

        function onAuthenticationSucceeded() {
            root.failed = false;
        }
    }

    SequentialAnimation {
        id: shakeAnimation
        NumberAnimation { target: root; property: "shakeOffset"; to: -8; duration: 35; easing.type: Easing.OutQuad }
        NumberAnimation { target: root; property: "shakeOffset"; to: 8; duration: 50; easing.type: Easing.InOutQuad }
        NumberAnimation { target: root; property: "shakeOffset"; to: 0; duration: 55; easing.type: Easing.OutQuad }
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
                anchors.horizontalCenterOffset: root.shakeOffset
                color: Theme.surface0
                border.color: root.failed ? Theme.red : Theme.surface2
                border.width: 1

                focus: true

                MouseArea { anchors.fill: parent; z: -1 }

                // Catches Escape/Enter at the card level regardless of which
                // child currently has focus (ported from Omarchy's keyCatcher).
                Item {
                    anchors.fill: parent
                    focus: true
                    Keys.priority: Keys.BeforeItem
                    Keys.onPressed: function(event) {
                        if (event.key === Qt.Key_Escape) {
                            root.cancel();
                            event.accepted = true;
                        } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                            root.submitResponse();
                            event.accepted = true;
                        }
                    }
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 10

                    Text {
                        Layout.fillWidth: true
                        visible: text.length > 0
                        text: Polkit.flow?.inputPrompt ?? ""
                        color: Theme.subtext0
                        wrapMode: Text.WordWrap
                        font.family: Config.fontFamily
                        font.pixelSize: Config.px(12)
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: Config.px(42)
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
                            color: root.failed ? Theme.red : Theme.text
                            selectionColor: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(14)
                            focus: true

                            Component.onCompleted: forceActiveFocus()
                            onAccepted: root.submitResponse()
                            Keys.onEscapePressed: root.cancel()
                        }

                        Text {
                            anchors.fill: field
                            visible: field.text.length === 0
                            text: root.failed ? "Wrong" : (root.submitted ? "Checking…" : "Enter password")
                            color: root.failed ? Theme.red : Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(13)
                        }
                    }
                }
            }

            // Justification pill floating above the card, matching Omarchy's
            // separate authorization label rather than cramming it into the card.
            Rectangle {
                width: Math.min(justificationText.implicitWidth + 24, win.width - 32)
                height: 28
                anchors.horizontalCenter: card.horizontalCenter
                anchors.bottom: card.top
                anchors.bottomMargin: 10
                radius: 14
                color: Theme.surface0

                Text {
                    id: justificationText
                    anchors.fill: parent
                    anchors.leftMargin: 12
                    anchors.rightMargin: 12
                    text: root.authorizationLabel(Polkit.flow?.message ?? "Authentication is required")
                    color: Theme.text
                    font.family: Config.fontFamily
                    font.pixelSize: Config.px(13)
                    font.bold: true
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                    elide: Text.ElideMiddle
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
