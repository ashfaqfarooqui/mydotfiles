pragma Singleton
import Quickshell

// A tiny shared channel so any bar widget can show a tooltip without each
// one needing its own popup surface. Bar.qml renders one small overlay
// window bound to this singleton's text/x. QtQuick.Controls (and its
// ToolTip type) isn't installed on this system (qt6-quickcontrols2 is not
// a quickshell dependency), so this is a from-scratch replacement.
//
// x is the hovered widget's scene-relative (i.e. window-relative) x
// position, passed by callers as `point.scenePosition.x` from their own
// HoverHandler — without it, the tooltip window (anchored only `top: true`,
// no `left`) always renders pinned at the screen's left edge regardless of
// which widget triggered it.
Singleton {
    property string text: ""
    property real x: 0

    function show(t, xPos) {
        text = t;
        if (xPos !== undefined) x = xPos;
    }

    function hide() {
        text = "";
    }
}
