import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.theme
import qs.config
import qs.services

// Calendar popup, opened by left-clicking ClockWidget.qml. Modeled on
// basecamp/omarchy's quattro-branch shell/plugins/panels/clock/Panel.qml: a
// fixed-height 6-row month grid (so the popup never resizes month to month),
// ISO week numbers in a left gutter, today rendered as an outline rather
// than a filled block, and keyboard-first navigation alongside the mouse.
// It's a pure date read-out, same as Omarchy's — no event/calendar-app
// integration.
Scope {
    id: root

    required property string screenName
    readonly property bool isActive: Calendar.panelVisible && Calendar.panelScreenName === screenName

    LazyLoader {
        active: root.isActive

        PanelWindow {
            id: win
            screen: Quickshell.screens.find(s => s.name === root.screenName) ?? Quickshell.screens[0]
            anchors { top: true; left: true }
            margins {
                top: Config.barHeight + 4
                left: Math.max(4, Math.round((screen.width - implicitWidth) / 2))
            }
            // Wide enough that the year view's 4-per-row mini-months (each
            // a 7-column day grid) have room to breathe — 340 was sized for
            // the single-month grid only and made year-view day numbers
            // cramped enough to overlap; 380, then 440, were still tight
            // once the mini-months grew roomier.
            implicitWidth: Config.px(500)
            implicitHeight: content.implicitHeight + 28
            color: "transparent"
            exclusiveZone: 0
            focusable: true

            readonly property date monthStart: new Date(Calendar.viewYear, Calendar.viewMonth, 1)
            readonly property bool onCurrentMonth: Calendar.viewYear === Calendar.today.getFullYear()
                && Calendar.viewMonth === Calendar.today.getMonth()

            // Mon-first (weekStartsMonday) or Sun-first grid, per
            // Settings.weekStartsMonday (toggled by clicking the week-number
            // gutter header below).
            readonly property int leadingBlanks: {
                const dow = monthStart.getDay(); // 0=Sun..6=Sat
                return Settings.weekStartsMonday ? (dow + 6) % 7 : dow;
            }
            readonly property int daysInMonth: new Date(Calendar.viewYear, Calendar.viewMonth + 1, 0).getDate()
            // Always 42 cells (6 rows x 7) so the grid height never changes
            // between months.
            readonly property var cells: {
                const arr = [];
                for (let i = 0; i < leadingBlanks; i++) arr.push(null);
                for (let d = 1; d <= daysInMonth; d++) arr.push(d);
                while (arr.length < 42) arr.push(null);
                return arr;
            }
            readonly property var dayHeaders: Settings.weekStartsMonday
                ? ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"]
                : ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]

            // ISO 8601 week number for the Monday (or Sunday-grid-adjusted
            // first day) of grid row `rowIndex`.
            function isoWeekNumber(rowIndex) {
                const firstCellOffset = rowIndex * 7 - leadingBlanks + 1;
                const d = new Date(Calendar.viewYear, Calendar.viewMonth, firstCellOffset);
                d.setHours(0, 0, 0, 0);
                d.setDate(d.getDate() + 3 - ((d.getDay() + 6) % 7));
                const week1 = new Date(d.getFullYear(), 0, 4);
                return 1 + Math.round(((d - week1) / 86400000 - 3 + ((week1.getDay() + 6) % 7)) / 7);
            }

            HyprlandFocusGrab {
                active: root.isActive
                windows: [win]
                onCleared: Calendar.hidePanel()
            }

            Rectangle {
                anchors.fill: parent
                radius: 12
                color: Theme.surface0
                border.color: Theme.surface2
                border.width: 1
                focus: true

                Keys.onEscapePressed: Calendar.hidePanel()
                Keys.onLeftPressed: Calendar.viewMode === "month" ? Calendar.prevMonth() : (Calendar.viewYear -= 1)
                Keys.onRightPressed: Calendar.viewMode === "month" ? Calendar.nextMonth() : (Calendar.viewYear += 1)
                Keys.onPressed: event => {
                    switch (event.key) {
                    case Qt.Key_BracketLeft: Calendar.prevMonth(); break;
                    case Qt.Key_BracketRight: Calendar.nextMonth(); break;
                    case Qt.Key_BraceLeft: Calendar.viewYear -= 1; break;
                    case Qt.Key_BraceRight: Calendar.viewYear += 1; break;
                    case Qt.Key_T: Calendar.resetToToday(); break;
                    case Qt.Key_W: Settings.weekStartsMonday = !Settings.weekStartsMonday; break;
                    case Qt.Key_Y: Calendar.toggleViewMode(); break;
                    default: return;
                    }
                    event.accepted = true;
                }

                MouseArea {
                    // Eats clicks landing inside the window but outside any
                    // control (same as every other panel in this repo).
                    anchors.fill: parent
                    z: -1
                    acceptedButtons: Qt.LeftButton
                    onWheel: wheel => {
                        if (Calendar.viewMode === "month") {
                            if (wheel.angleDelta.y < 0) Calendar.nextMonth();
                            else if (wheel.angleDelta.y > 0) Calendar.prevMonth();
                        } else {
                            if (wheel.angleDelta.y < 0) Calendar.viewYear += 1;
                            else if (wheel.angleDelta.y > 0) Calendar.viewYear -= 1;
                        }
                    }
                }

                // One compact month for the year-overview grid below: month
                // name + a small day-number grid, no week-number gutter.
                // Reuses the same leading-blanks/days-in-month math as the
                // full month grid above, scoped to whatever y/m it's given
                // rather than the panel's own Calendar.viewMonth.
                component MiniMonth: Rectangle {
                    id: mini
                    // Named year/month, not y/m — Rectangle already has a
                    // built-in `y` (its vertical position), which a same-
                    // named property would silently shadow.
                    required property int year
                    required property int month
                    signal picked()
                    color: "transparent"
                    radius: 6

                    readonly property date start: new Date(year, month, 1)
                    readonly property bool isCurrentMonth: year === Calendar.today.getFullYear() && month === Calendar.today.getMonth()
                    readonly property int leadingBlanks: {
                        const dow = start.getDay();
                        return Settings.weekStartsMonday ? (dow + 6) % 7 : dow;
                    }
                    readonly property int daysInMonth: new Date(year, month + 1, 0).getDate()
                    readonly property var cells: {
                        const arr = [];
                        for (let i = 0; i < leadingBlanks; i++) arr.push(null);
                        for (let d = 1; d <= daysInMonth; d++) arr.push(d);
                        return arr;
                    }

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: mini.picked()
                    }

                    ColumnLayout {
                        anchors.fill: parent
                        anchors.margins: 7
                        spacing: 6

                        Text {
                            Layout.alignment: Qt.AlignHCenter
                            text: Qt.formatDate(mini.start, "MMM")
                            color: mini.isCurrentMonth ? Theme.blue : Theme.subtext1
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(13)
                        }

                        // Plain Grid, not GridLayout — GridLayout sizes each
                        // column from the *natural* content width of items
                        // in it, and fillWidth only grows a column beyond
                        // that if there's leftover space, which doesn't
                        // guarantee 7 identical column widths. Grid just
                        // positions items at whatever width they're given,
                        // so giving every day cell the same explicit
                        // (mini.width-based) width makes the 7 columns
                        // genuinely even.
                        Grid {
                            id: dayGrid
                            // Explicit width from mini.width (set
                            // externally by the year-grid delegate below),
                            // not Layout.fillWidth — a Grid's own natural
                            // width is computed FROM its children's widths,
                            // so children whose width formula referenced
                            // dayGrid.width were a genuine binding loop that
                            // collapsed the whole panel's height to an
                            // invalid value and made it stop rendering.
                            width: mini.width - 14
                            columns: 7
                            rowSpacing: 3
                            columnSpacing: 2

                            Repeater {
                                model: mini.cells
                                delegate: Text {
                                    required property var modelData
                                    required property int index
                                    width: (dayGrid.width - dayGrid.columnSpacing * 6) / 7
                                    height: 20
                                    horizontalAlignment: Text.AlignHCenter
                                    verticalAlignment: Text.AlignVCenter
                                    text: modelData ?? ""
                                    readonly property bool isToday: mini.isCurrentMonth && modelData === Calendar.today.getDate()
                                    color: isToday ? Theme.blue : Theme.subtext0
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(10)
                                    font.bold: isToday
                                }
                            }
                        }
                    }
                }

                ColumnLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 10

                    // Hero row: big current-date readout, click to jump back
                    // to today (only meaningful once you've navigated away).
                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            // nf-md-calendar_month — the previous \u{F0075}
                            // was actually nf-md-incognito_off (confirmed
                            // against nerd-fonts' glyphnames.json), which is
                            // why it read as an incognito symbol.
                            text: "\u{F0E17}"
                            color: Theme.blue
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(22)
                        }

                        Text {
                            text: Qt.formatDate(Calendar.today, "dddd, MMMM d")
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(16)
                            Layout.fillWidth: true
                            elide: Text.ElideRight
                        }

                        MouseArea {
                            Layout.preferredWidth: 24
                            Layout.preferredHeight: 24
                            enabled: !win.onCurrentMonth
                            cursorShape: enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
                            onClicked: Calendar.resetToToday()

                            Text {
                                anchors.centerIn: parent
                                visible: !win.onCurrentMonth
                                text: "\u{F0450}"
                                color: Theme.subtext0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(14)
                            }
                        }
                    }

                    // Month/year nav: chevrons flanking a fixed-width centered
                    // label so they don't shift position between "May 2026"
                    // and "September 2026" (or between years), plus a toggle
                    // to switch between the single-month grid and a 12-up
                    // year overview.
                    RowLayout {
                        Layout.fillWidth: true

                        Text {
                            text: "\u{F0141}"
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(14)
                            MouseArea {
                                anchors.fill: parent
                                anchors.margins: -4
                                onClicked: Calendar.viewMode === "month" ? Calendar.prevMonth() : (Calendar.viewYear -= 1)
                            }
                        }

                        Text {
                            Layout.fillWidth: true
                            horizontalAlignment: Text.AlignHCenter
                            text: Calendar.viewMode === "month" ? Qt.formatDate(win.monthStart, "MMMM yyyy") : String(Calendar.viewYear)
                            color: Theme.text
                            font.family: Config.fontFamily
                            font.bold: true
                            font.pixelSize: Config.px(13)
                        }

                        Text {
                            text: "\u{F0142}"
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(14)
                            MouseArea {
                                anchors.fill: parent
                                anchors.margins: -4
                                onClicked: Calendar.viewMode === "month" ? Calendar.nextMonth() : (Calendar.viewYear += 1)
                            }
                        }

                        // nf-md-calendar_multiple / nf-md-calendar_month —
                        // toggles the single-month grid vs the 12-up year
                        // overview below.
                        Text {
                            Layout.leftMargin: 6
                            text: Calendar.viewMode === "month" ? "\u{F00F1}" : "\u{F0E17}"
                            color: Theme.subtext1
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(14)
                            MouseArea { anchors.fill: parent; anchors.margins: -4; onClicked: Calendar.toggleViewMode() }
                        }
                    }

                    // Week-number gutter + day-of-week header + 6-row grid.
                    GridLayout {
                        visible: Calendar.viewMode === "month"
                        Layout.fillWidth: true
                        columns: 8
                        rowSpacing: 4
                        columnSpacing: 4

                        // Gutter header: click toggles Mon/Sun week start.
                        // Every delegate below sets Layout.row/Layout.column
                        // explicitly — GridLayout only auto-flows items in
                        // strict row-major declaration order, which breaks
                        // down the moment more than one Repeater feeds the
                        // same GridLayout (the week-number column would
                        // otherwise smear across a single row instead of
                        // landing one-per-row).
                        Text {
                            text: "Wk"
                            color: Theme.overlay0
                            font.family: Config.fontFamily
                            font.pixelSize: Config.px(9)
                            Layout.row: 0
                            Layout.column: 0
                            Layout.preferredWidth: 22
                            horizontalAlignment: Text.AlignHCenter

                            MouseArea {
                                anchors.fill: parent
                                anchors.margins: -3
                                cursorShape: Qt.PointingHandCursor
                                onClicked: Settings.weekStartsMonday = !Settings.weekStartsMonday
                            }
                        }

                        Repeater {
                            model: win.dayHeaders
                            delegate: Text {
                                required property string modelData
                                required property int index
                                text: modelData
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(10)
                                Layout.row: 0
                                Layout.column: index + 1
                                Layout.fillWidth: true
                                horizontalAlignment: Text.AlignHCenter
                            }
                        }

                        Repeater {
                            model: 6
                            delegate: Text {
                                required property int index
                                text: win.isoWeekNumber(index)
                                color: Theme.overlay0
                                font.family: Config.fontFamily
                                font.pixelSize: Config.px(9)
                                Layout.row: index + 1
                                Layout.column: 0
                                Layout.preferredWidth: 22
                                Layout.preferredHeight: 26
                                horizontalAlignment: Text.AlignHCenter
                                verticalAlignment: Text.AlignVCenter
                            }
                        }

                        Repeater {
                            model: win.cells
                            delegate: Item {
                                required property var modelData
                                required property int index
                                Layout.row: Math.floor(index / 7) + 1
                                Layout.column: (index % 7) + 1
                                Layout.fillWidth: true
                                Layout.preferredHeight: 26

                                readonly property bool inMonth: modelData !== null
                                readonly property bool isToday: inMonth && win.onCurrentMonth && modelData === Calendar.today.getDate()
                                readonly property int dow: (index + (Settings.weekStartsMonday ? 0 : 1)) % 7
                                readonly property bool isWeekend: Settings.weekStartsMonday ? (dow === 5 || dow === 6) : (dow === 0 || dow === 6)

                                Rectangle {
                                    visible: parent.isToday
                                    anchors.centerIn: parent
                                    width: 22
                                    height: 22
                                    radius: 11
                                    color: "transparent"
                                    border.color: Theme.blue
                                    border.width: 1.5
                                }

                                Text {
                                    visible: parent.inMonth
                                    anchors.centerIn: parent
                                    text: parent.modelData ?? ""
                                    color: parent.isToday ? Theme.blue : (parent.isWeekend ? Theme.overlay0 : Theme.text)
                                    font.family: Config.fontFamily
                                    font.pixelSize: Config.px(11)
                                    font.bold: parent.isToday
                                }
                            }
                        }
                    }

                    // Year overview: 12 mini-months, 4 per row. Plain Grid,
                    // not GridLayout — see dayGrid's comment above for why:
                    // an explicit width on every MiniMonth (from content.width,
                    // not yearGrid's own — same binding-loop trap as dayGrid)
                    // guarantees 4 genuinely even columns instead of relying
                    // on GridLayout's per-column content-based sizing.
                    Grid {
                        id: yearGrid
                        visible: Calendar.viewMode === "year"
                        width: content.width
                        columns: 4
                        rowSpacing: 10
                        columnSpacing: 10

                        Repeater {
                            model: 12
                            delegate: MiniMonth {
                                required property int index
                                width: (yearGrid.width - yearGrid.columnSpacing * 3) / 4
                                height: 142
                                year: Calendar.viewYear
                                month: index
                                onPicked: Calendar.jumpToMonth(index)
                            }
                        }
                    }
                }
            }
        }
    }
}
