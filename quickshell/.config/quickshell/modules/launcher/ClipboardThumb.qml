import QtQuick
import qs.services

// Thin display for one clipboard image thumbnail. Decoding/caching now
// lives in Cliphist.requestThumb()/thumbCache (see services/Cliphist.qml) so
// scrolling this row back into view after it's already been decoded once is
// a plain map lookup, not a new subprocess.
Item {
    id: root
    required property string entryId

    function ensureRequested() {
        if (entryId) Cliphist.requestThumb(entryId);
    }

    Component.onCompleted: ensureRequested()
    onEntryIdChanged: ensureRequested()

    Image {
        anchors.fill: parent
        source: Cliphist.thumbCache[root.entryId] ?? ""
        fillMode: Image.PreserveAspectCrop
        asynchronous: true
        cache: true
        sourceSize.width: 64
        sourceSize.height: 64
    }
}
