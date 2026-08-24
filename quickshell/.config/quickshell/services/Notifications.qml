pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Services.Notifications

// Replaces swaync's daemon role (org.freedesktop.Notifications owner) +
// swaync-client's -swb badge/DND query. NotificationServer does not
// auto-persist anything — a notification is dropped the instant the
// `notification` signal handler returns unless `tracked` is explicitly set
// true, and `trackedNotifications` is then the *only* list it keeps, so
// this is the single source of truth for both the toast popup queue and
// the control-center history list (no separate duplicated model).
Singleton {
    id: root

    property bool dndEnabled: false
    // Recently-arrived notifications still shown as a toast, keyed by id,
    // auto-removed by their own per-notification timer.
    property var popupQueue: []
    signal popupQueueChanged_()

    // One live auto-dismiss Timer per notification id. A sender replacing an
    // existing notification (same id — e.g. a repeated volume/progress
    // toast) re-fires the `notification` signal with a new object; without
    // tracking timers by id, the OLD timer would still be holding a
    // reference to the now-replaced object, so its `.filter(n => n !==
    // notification)` on expiry would never match the NEW object actually
    // sitting in popupQueue — leaving that toast stuck on screen forever.
    // Keying by id lets a replace cancel and replace the old timer outright.
    property var _timers: ({})

    readonly property int unreadCount: server.trackedNotifications.values.length

    // Notification objects carry no timestamp of their own (confirmed
    // against the installed qmltypes — id/appName/summary/body/etc but
    // nothing time-related), so "received at" has to be tracked here for
    // the control-center history's relative-time labels.
    property var _receivedAt: ({})
    function receivedAt(id) {
        return root._receivedAt[id] ?? Date.now();
    }

    // Cross-directory toggle channel: NotificationBadge.qml (modules/bar)
    // and ControlCenter.qml (modules/notifications) have no direct
    // reference to each other's instance, so the shared visibility flag
    // lives here — same pattern as TooltipBus for the bar's tooltips.
    property bool controlCenterVisible: false

    function toggleControlCenter() {
        controlCenterVisible = !controlCenterVisible;
    }

    function hideControlCenter() {
        controlCenterVisible = false;
    }

    function toggleDnd() {
        dndEnabled = !dndEnabled;
    }

    function clearAll() {
        const list = server.trackedNotifications.values.slice();
        for (const n of list) n.dismiss();
    }

    function dismiss(notification) {
        notification.dismiss();
    }

    NotificationServer {
        id: server
        bodySupported: true
        bodyMarkupSupported: true
        imageSupported: true
        actionsSupported: true
        actionIconsSupported: true
        persistenceSupported: true
        keepOnReload: true

        onNotification: notification => {
            // tracked = true is required just to keep the object alive long
            // enough to render as a toast (see the class comment above) —
            // but a sender-marked `transient` notification (voxtype's
            // "Transcribed" toast: hints={transient: true,
            // x-canonical-private-synchronous: "voxtype"}, confirmed via
            // dbus-monitor) explicitly asks not to be kept around after it's
            // shown. Leaving tracked true for those meant every dictation
            // permanently inflated the notification badge count and control
            // -center history — they never expired from anywhere but the
            // toast queue. Non-transient notifications still behave as
            // before: they linger in trackedNotifications (control-center
            // history) after their toast times out, until dismissed.
            notification.tracked = true;
            root._receivedAt[notification.id] = Date.now();

            if (!root.dndEnabled) {
                // expire_timeout === 0 is the sender explicitly asking to
                // never auto-expire (freedesktop notification spec) — e.g.
                // a critical alert meant to stay until dismissed. -1 (or
                // anything else <= 0 that isn't 0) means "use the server
                // default", which is the 8s fallback. Quickshell's
                // Notification.expireTimeout is already in milliseconds
                // (confirmed empirically: notify-send -t 5000 arrives as
                // expireTimeout=5000, not 5) despite what the docs site's
                // prose implies — do not re-multiply by 1000 here, that was
                // turning a 5s hyprshot toast into an 83-minute one.
                const neverExpires = notification.expireTimeout === 0;
                const timeoutMs = notification.expireTimeout > 0 ? notification.expireTimeout : 8000;

                // A replace (same id as an already-showing toast) drops the
                // old entry and its timer before re-adding — otherwise the
                // stale timer's object reference never matches the new one
                // in popupQueue and that toast never clears (see _timers'
                // comment above).
                const existingTimer = root._timers[notification.id];
                if (existingTimer) {
                    existingTimer.destroy();
                    delete root._timers[notification.id];
                }
                root.popupQueue = [...root.popupQueue.filter(n => n.id !== notification.id), notification];
                root.popupQueueChanged_();

                if (!neverExpires) {
                    const t = popupTimerComponent.createObject(root, { interval: timeoutMs, running: true });
                    root._timers[notification.id] = t;
                    t.triggered.connect(() => {
                        root.popupQueue = root.popupQueue.filter(n => n.id !== notification.id);
                        root.popupQueueChanged_();
                        if (notification.transient) notification.dismiss();
                        delete root._timers[notification.id];
                        t.destroy();
                    });
                }

                notification.closed.connect(() => {
                    root.popupQueue = root.popupQueue.filter(n => n.id !== notification.id);
                    root.popupQueueChanged_();
                    const timer = root._timers[notification.id];
                    if (timer) {
                        timer.destroy();
                        delete root._timers[notification.id];
                    }
                    delete root._receivedAt[notification.id];
                });
            } else if (notification.transient) {
                notification.tracked = false;
            }
        }
    }

    readonly property var trackedNotifications: server.trackedNotifications

    Component {
        id: popupTimerComponent
        Timer { repeat: false }
    }
}
