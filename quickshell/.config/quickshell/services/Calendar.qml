pragma Singleton
import QtQuick
import Quickshell
import qs.services

// Visibility + navigation state for CalendarPanel.qml (modules/bar/),
// opened by left-clicking ClockWidget.qml. Same panelVisible/panelScreenName
// toggle pattern as Network.qml/Battery.qml/etc — see Network.qml for why
// the per-screen gating is mandatory. Month/year navigation state lives here
// rather than in the panel itself so it survives the panel's LazyLoader
// tearing the popup down between opens (reopening always starts back at the
// current month, per Omarchy's own "read-out, not a picker" design).
Singleton {
    id: root

    readonly property date today: new Date()
    property int viewYear: today.getFullYear()
    property int viewMonth: today.getMonth()
    // "month" (single-month grid, as before) or "year" (12 mini-months).
    property string viewMode: "month"

    function resetToToday() {
        viewYear = today.getFullYear();
        viewMonth = today.getMonth();
        viewMode = "month";
    }

    function toggleViewMode() {
        viewMode = viewMode === "month" ? "year" : "month";
    }

    // Used by the year view's mini-months to jump straight into a month.
    function jumpToMonth(month) {
        viewMonth = month;
        viewMode = "month";
    }

    function nextMonth() {
        if (viewMonth === 11) { viewMonth = 0; viewYear += 1; }
        else viewMonth += 1;
    }

    function prevMonth() {
        if (viewMonth === 0) { viewMonth = 11; viewYear -= 1; }
        else viewMonth -= 1;
    }

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
            SystemStats.hidePanel();
            AgentsUsage.hidePanel();
            resetToToday();
            panelScreenName = screenName;
            panelVisible = true;
        }
    }

    function hidePanel() {
        panelVisible = false;
    }
}
