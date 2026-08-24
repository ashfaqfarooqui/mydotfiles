pragma Singleton
import QtQuick
import Quickshell
import qs.services

// Replaces waybar's custom/hypridle module. hypridle itself is gone (see
// services/Idle.qml, which now owns the actual timeout listeners) — this is
// now a thin bar-widget-facing view over Idle.stayAwake, kept as its own
// singleton only so IdleToggle.qml's existing active/tooltip/toggle()
// contract doesn't need to change.
Singleton {
    id: root
    readonly property bool active: Idle.idleEnabled
    readonly property string tooltip: Idle.tooltipText()

    function toggle() {
        Idle.toggle();
    }
}
