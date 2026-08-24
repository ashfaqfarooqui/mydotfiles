import QtQuick
import QtQuick.Effects
import qs.theme
import qs.config

// Lock-screen surface content, instantiated inside Lock.qml's
// WlSessionLockSurface. Styled to match BatteryPanel.qml/BrightnessPanel.qml's
// existing chip/slider look (Theme.surface0/text/red tokens) rather than
// hyprlock.conf's Catppuccin-hardcoded input-field, so it stays in sync with
// theme/theme.json's live palette the same way every other quickshell popup
// already does.
Item {
    id: root

    property string wallpaperPath: ""
    property real blurAmount: 64
    property bool authenticating: false
    property string failureMessage: ""

    signal submitPassword(string password)

    readonly property bool errorState: failureMessage.length > 0

    Rectangle {
        anchors.fill: parent
        color: Theme.crust

        Image {
            id: wallpaper
            anchors.fill: parent
            source: root.wallpaperPath ? "file://" + root.wallpaperPath : ""
            fillMode: Image.PreserveAspectCrop
            asynchronous: true
            cache: false
            sourceSize.width: width
            sourceSize.height: height
        }

        MultiEffect {
            anchors.fill: wallpaper
            source: wallpaper
            autoPaddingEnabled: false
            blurEnabled: wallpaper.status === Image.Ready
            blur: 1.0
            blurMax: root.blurAmount
            blurMultiplier: 1.0
        }

        Rectangle {
            anchors.fill: parent
            color: Theme.crust
            opacity: 0.35
        }

        Column {
            anchors.top: parent.top
            anchors.right: parent.right
            anchors.topMargin: 60
            anchors.rightMargin: 60
            spacing: 4

            Text {
                anchors.right: parent.right
                text: clock.text
                color: Theme.text
                font.family: Config.fontFamily
                font.pixelSize: 72
                font.bold: true

                Timer {
                    id: clockTimer
                    interval: 1000
                    running: true
                    repeat: true
                    triggeredOnStart: true
                    onTriggered: clock.text = Qt.formatTime(new Date(), "HH:mm")
                }
                Text { id: clock; visible: false }
            }

            Text {
                anchors.right: parent.right
                text: Qt.formatDate(new Date(), "dddd, d MMMM yyyy")
                color: Theme.subtext1
                font.family: Config.fontFamily
                font.pixelSize: 20
            }
        }

        Rectangle {
            id: inputField
            width: 340
            height: 56
            radius: 12
            anchors.centerIn: parent
            color: Theme.surface0
            border.width: 2
            border.color: root.errorState ? Theme.red : Theme.blue
            clip: true

            TextInput {
                id: passwordInput
                anchors.fill: parent
                anchors.leftMargin: 18
                anchors.rightMargin: 18
                verticalAlignment: TextInput.AlignVCenter
                echoMode: TextInput.Password
                passwordCharacter: "●"
                color: Theme.text
                selectionColor: Theme.blue
                font.family: Config.fontFamily
                font.pixelSize: 20
                focus: true

                Component.onCompleted: forceActiveFocus()

                onAccepted: {
                    const value = text;
                    text = "";
                    if (value.length > 0) root.submitPassword(value);
                }
            }

            Text {
                anchors.fill: passwordInput
                visible: passwordInput.text.length === 0
                text: root.authenticating ? "Checking…" : (root.errorState ? root.failureMessage : "Enter password")
                color: root.errorState ? Theme.red : Theme.overlay0
                font.family: Config.fontFamily
                font.pixelSize: 16
                font.italic: root.errorState
                verticalAlignment: Text.AlignVCenter
            }
        }

        MouseArea {
            anchors.fill: parent
            z: -1
            onClicked: passwordInput.forceActiveFocus()
        }
    }

    // Failure clears the input from the driving PamContext's own reset, but
    // the field only hears about it through failureMessage — clear locally
    // whenever a new failure message lands so the box never carries the
    // rejected password forward.
    onFailureMessageChanged: if (failureMessage.length > 0) passwordInput.text = ""
    onVisibleChanged: if (visible) passwordInput.forceActiveFocus()
}
