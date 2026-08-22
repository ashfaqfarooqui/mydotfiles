pragma Singleton
import Quickshell

// A tiny shared channel so any bar widget can show a tooltip without each
// one needing its own popup surface. Bar.qml renders one small overlay
// window bound to this singleton's text/screen. QtQuick.Controls (and its
// ToolTip type) isn't installed on this system (qt6-quickcontrols2 is not
// a quickshell dependency), so this is a from-scratch replacement.
Singleton {
    property string text: ""

    function show(t) {
        text = t;
    }

    function hide() {
        text = "";
    }
}
