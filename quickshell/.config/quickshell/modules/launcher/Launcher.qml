import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Widgets
import qs.theme
import qs.config
import qs.services

// Replaces `rofi -show drun` (SUPER+SPACE, hypr/.config/hypr/conf/
// keybindings.lua:5). Uses Quickshell's own DesktopEntries index and
// iconPath() resolver natively instead of a hand-rolled XDG scan +
// icon-theme walk — both already exist in Quickshell core and follow the
// freedesktop desktop-entry/icon-theme specs more completely than a
// from-scratch reimplementation would (see quickshell.org docs for
// DesktopEntries/DesktopEntry/Quickshell.iconPath). DesktopEntries.applications
// is already filtered for Hidden/NoDisplay.
//
// Pin/usage state (~/.local/state/quickshell/launcher-usage.json) is a new
// capability rofi's config never had — no frecency cache to port, this is
// user-requested net-new behavior. Pinned apps always sort first; the rest
// sort by usage count then recency, falling back to alphabetical for
// anything never launched. That ordering only applies to the empty-query
// "browse" view — while actively filtering, results stay in relevance
// (plain array) order so text relevance isn't fought by frecency.
Scope {
    id: root

    property var usageData: ({ pinned: [], usage: {} })

    readonly property var entries: DesktopEntries.applications.values

    FileView {
        id: usageFile
        path: Quickshell.env("HOME") + "/.local/state/quickshell/launcher-usage.json"
        watchChanges: false

        onLoaded: {
            try {
                const parsed = JSON.parse(text());
                root.usageData = {
                    pinned: parsed.pinned ?? [],
                    usage: parsed.usage ?? {},
                };
            } catch (e) {
                root.usageData = { pinned: [], usage: {} };
            }
        }
        onLoadFailed: root.usageData = { pinned: [], usage: {} }
    }

    function persistUsage() {
        usageFile.setText(JSON.stringify(root.usageData));
    }

    function recordLaunch(appId) {
        if (!appId) return;
        const usage = Object.assign({}, root.usageData.usage);
        const prev = usage[appId] ?? { count: 0, lastUsed: 0 };
        usage[appId] = { count: prev.count + 1, lastUsed: Date.now() };
        root.usageData = { pinned: root.usageData.pinned, usage: usage };
        persistUsage();
    }

    function togglePin(appId) {
        if (!appId) return;
        const pinned = root.usageData.pinned.slice();
        const idx = pinned.indexOf(appId);
        if (idx >= 0) pinned.splice(idx, 1);
        else pinned.push(appId);
        root.usageData = { pinned: pinned, usage: root.usageData.usage };
        persistUsage();
    }

    // Pinned first (in pinned-array order), then by usage count desc /
    // lastUsed desc, then alphabetical for never-launched apps.
    function sortedForBrowse(list) {
        const pinned = root.usageData.pinned;
        const usage = root.usageData.usage;
        const pinnedApps = [];
        const restApps = [];
        for (const a of list) {
            if (pinned.includes(a.id)) pinnedApps.push(a);
            else restApps.push(a);
        }
        pinnedApps.sort((a, b) => pinned.indexOf(a.id) - pinned.indexOf(b.id));
        restApps.sort((a, b) => {
            const ua = usage[a.id];
            const ub = usage[b.id];
            const ca = ua?.count ?? 0;
            const cb = ub?.count ?? 0;
            if (ca !== cb) return cb - ca;
            const la = ua?.lastUsed ?? 0;
            const lb = ub?.lastUsed ?? 0;
            if (la !== lb) return lb - la;
            return a.name.localeCompare(b.name);
        });
        return pinnedApps.concat(restApps);
    }

    LazyLoader {
        active: LauncherBus.launcherVisible

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === Hypr.focusedMonitor?.name) ?? Quickshell.screens[0]
            anchors { top: true }
            margins.top: Math.round(screen.height * 0.15)
            implicitWidth: 480
            implicitHeight: 420
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            Component.onCompleted: search.forceActiveFocus()

            readonly property var filtered: {
                const q = search.text.trim().toLowerCase();
                const list = root.entries;
                if (!q) return root.sortedForBrowse(list);
                return list.filter(a =>
                    a.name.toLowerCase().includes(q) ||
                    (a.genericName ?? "").toLowerCase().includes(q) ||
                    (a.keywords ?? []).some(k => k.toLowerCase().includes(q))
                );
            }

            // DesktopEntry.execute() ignores runInTerminal and field codes
            // per quickshell.org docs, so terminal apps still need manual
            // wrapping; non-terminal apps use execute() directly rather
            // than re-deriving an exec string (handles quoting/edge cases
            // execString's raw form doesn't).
            function launch(app) {
                if (!app) return;
                if (app.runInTerminal) {
                    const cmd = app.execString.replace(/%[fFuUick]/g, "").trim();
                    Quickshell.execDetached(["kitty", "-e", "sh", "-c", cmd]);
                } else {
                    app.execute();
                }
                root.recordLaunch(app.id);
                LauncherBus.launcherVisible = false;
                search.text = "";
                list.currentIndex = 0;
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: 36
                        radius: 8
                        color: Theme.surface1

                        Text {
                            visible: search.text.length === 0
                            anchors.verticalCenter: parent.verticalCenter
                            anchors.left: parent.left
                            anchors.leftMargin: 10
                            text: "Search apps…"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: 14
                        }

                        TextInput {
                            id: search
                            anchors.fill: parent
                            anchors.leftMargin: 10
                            anchors.rightMargin: 10
                            verticalAlignment: TextInput.AlignVCenter
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.pixelSize: 14
                            focus: true
                            clip: true

                            Keys.onEscapePressed: LauncherBus.launcherVisible = false
                            Keys.onDownPressed: list.incrementCurrentIndex()
                            Keys.onUpPressed: list.decrementCurrentIndex()
                            Keys.onReturnPressed: win.launch(win.filtered[list.currentIndex])
                            Keys.onPressed: event => {
                                if (event.key === Qt.Key_P && (event.modifiers & Qt.ControlModifier)) {
                                    const app = win.filtered[list.currentIndex];
                                    if (app) root.togglePin(app.id);
                                    event.accepted = true;
                                }
                            }
                        }
                    }

                    ListView {
                        id: list
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        model: win.filtered
                        currentIndex: 0

                        delegate: Rectangle {
                            id: delegateRoot
                            required property var modelData
                            required property int index
                            width: list.width
                            height: 34
                            radius: 6
                            color: index === list.currentIndex ? Theme.surface2 : "transparent"

                            readonly property bool pinned: root.usageData.pinned.includes(modelData.id)
                            readonly property string iconSource: modelData.icon ? Quickshell.iconPath(modelData.icon, true) : ""

                            RowLayout {
                                anchors.fill: parent
                                anchors.leftMargin: 10
                                anchors.rightMargin: 10
                                spacing: 8

                                IconImage {
                                    visible: delegateRoot.iconSource !== ""
                                    source: delegateRoot.iconSource
                                    implicitSize: 20
                                    asynchronous: true
                                }

                                Text {
                                    text: modelData.name
                                    color: Theme.text
                                    font.family: Config.fontFamily
                                    font.pixelSize: 13
                                    Layout.fillWidth: true
                                    elide: Text.ElideRight
                                }

                                Text {
                                    visible: delegateRoot.pinned
                                    text: "" // Font Awesome thumb-tack, Nerd Fonts fa-set codepoint
                                    color: Theme.yellow
                                    font.family: Config.fontFamily
                                    font.pixelSize: 12
                                }
                            }

                            MouseArea {
                                anchors.fill: parent
                                acceptedButtons: Qt.LeftButton | Qt.RightButton
                                hoverEnabled: true
                                onClicked: mouse => {
                                    if (mouse.button === Qt.RightButton) root.togglePin(modelData.id);
                                    else win.launch(modelData);
                                }
                                onEntered: list.currentIndex = index
                            }
                        }
                    }
                }
            }
        }
    }
}
