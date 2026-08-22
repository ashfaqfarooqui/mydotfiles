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

    readonly property int unreadCount: server.trackedNotifications.values.length

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
            notification.tracked = true;

            if (!root.dndEnabled) {
                const timeoutMs = notification.expireTimeout > 0 ? notification.expireTimeout * 1000 : 8000;
                root.popupQueue = [...root.popupQueue, notification];
                root.popupQueueChanged_();

                const t = popupTimerComponent.createObject(root, { interval: timeoutMs, running: true });
                t.triggered.connect(() => {
                    root.popupQueue = root.popupQueue.filter(n => n !== notification);
                    root.popupQueueChanged_();
                    t.destroy();
                });

                notification.closed.connect(() => {
                    root.popupQueue = root.popupQueue.filter(n => n !== notification);
                    root.popupQueueChanged_();
                });
            }
        }
    }

    readonly property var trackedNotifications: server.trackedNotifications

    Component {
        id: popupTimerComponent
        Timer { repeat: false }
    }
}
