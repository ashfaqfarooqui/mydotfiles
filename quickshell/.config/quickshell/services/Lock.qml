pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Pam
import Quickshell.Wayland
import qs.modules.lock
import qs.services

// Replaces hyprlock. Uses Quickshell.Wayland.WlSessionLock (ext-session-lock-v1)
// plus two concurrent PamContexts, mirroring basecamp/omarchy's dual-PAM lock
// (shell/plugins/lock/Service.qml on the quattro branch): a fingerprint
// PamContext runs continuously in the background for the whole lock session,
// retrying itself on every failure/timeout, while a separate password
// PamContext only runs per submitted attempt. The two are fully decoupled —
// neither can block or disable the other — so a slow/timed-out fingerprint
// scan never locks the user out of typing a password, which a single shared
// PAM stack (fingerprint-then-password fallback) did.
Singleton {
    id: root

    readonly property string userName: Quickshell.env("USER") || Quickshell.env("LOGNAME")
    // Configurable via modules/launcher/LockAppearancePicker.qml, persisted
    // in Settings.qml so a wallpaper/blur change survives a shell restart.
    readonly property string wallpaperPath: Settings.lockWallpaper
    readonly property real blurAmount: Settings.lockBlur

    property bool lockRequested: false
    property bool authenticatingPassword: false
    property bool fingerprintActive: false
    property bool fingerprintConfigured: false
    property string failureMessage: ""
    property int failedAttempts: 0
    property string pendingPassword: ""

    readonly property bool locked: lockRequested || sessionLock.locked

    function beginLock() {
        if (locked) return;
        resetAuthState();
        lockRequested = true;
        sessionLock.locked = true;
    }

    function finishUnlock() {
        lockRequested = false;
        resetAuthState();
        sessionLock.locked = false;
    }

    function resetAuthState() {
        failureMessage = "";
        failedAttempts = 0;
        authenticatingPassword = false;
        pendingPassword = "";
        fingerprintRetryTimer.stop();
        if (passwordPam.active) passwordPam.abort();
        if (fingerprintPam.active) fingerprintPam.abort();
    }

    // Started once the session lock surface is secure, and re-armed after
    // every failed/timed-out scan for as long as the screen stays locked —
    // touch the sensor at any point, no need to wait for a prompt.
    function startFingerprint() {
        if (!lockRequested || !fingerprintConfigured || fingerprintPam.active) return;
        fingerprintActive = true;
        if (!fingerprintPam.start()) fingerprintActive = false;
    }

    function submitPassword(password) {
        if (!lockRequested || authenticatingPassword || password.length === 0) return;
        pendingPassword = password;
        failureMessage = "";
        authenticatingPassword = true;
        if (!passwordPam.start()) handlePasswordFailure();
    }

    function handlePasswordFailure() {
        authenticatingPassword = false;
        pendingPassword = "";
        failedAttempts += 1;
        failureMessage = "Authentication failed (" + failedAttempts + ")";
    }

    PamContext {
        id: passwordPam
        config: "quickshell-lock-password"
        user: root.userName

        onResponseRequiredChanged: {
            if (responseRequired) respond(root.pendingPassword);
        }

        onCompleted: result => {
            root.authenticatingPassword = false;
            root.pendingPassword = "";
            if (!root.lockRequested) return;
            if (result === PamResult.Success) root.finishUnlock();
            else root.handlePasswordFailure();
        }

        onError: error => root.handlePasswordFailure()
    }

    PamContext {
        id: fingerprintPam
        config: "quickshell-lock-fingerprint"
        user: root.userName

        onCompleted: result => {
            root.fingerprintActive = false;
            if (!root.lockRequested) return;
            if (result === PamResult.Success) root.finishUnlock();
            else fingerprintRetryTimer.restart();
        }

        onError: error => {
            root.fingerprintActive = false;
            if (root.lockRequested) fingerprintRetryTimer.restart();
        }
    }

    Timer {
        id: fingerprintRetryTimer
        interval: 300
        repeat: false
        onTriggered: root.startFingerprint()
    }

    // Only scan when a fingerprint PAM stack actually exists — otherwise
    // fingerprintPam.start() would fail instantly and this would spin the
    // retry timer forever for no reason.
    FileView {
        path: "/etc/pam.d/quickshell-lock-fingerprint"
        watchChanges: true
        printErrors: false
        onLoaded: root.fingerprintConfigured = true
        onLoadFailed: root.fingerprintConfigured = false
    }

    onFingerprintConfiguredChanged: if (fingerprintConfigured) startFingerprint()

    WlSessionLock {
        id: sessionLock
        locked: false

        onSecureStateChanged: {
            if (secure) root.startFingerprint();
        }

        onLockStateChanged: {
            // A lock cleared from outside this singleton (e.g. a TTY
            // failsafe) must still reset our own request/auth state.
            if (!locked && root.lockRequested) {
                root.lockRequested = false;
                root.resetAuthState();
            }
        }

        WlSessionLockSurface {
            LockScreen {
                anchors.fill: parent
                wallpaperPath: root.wallpaperPath
                blurAmount: root.blurAmount
                authenticating: root.authenticatingPassword
                failureMessage: root.failureMessage
                onSubmitPassword: password => root.submitPassword(password)
            }
        }
    }
}
