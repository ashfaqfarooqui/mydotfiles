pragma Singleton
import QtQuick
import Quickshell

// Visibility toggle channel for the six Phase 3 popups (Launcher,
// WindowSwitcher, ClipboardPicker, Cheatsheet, ThemePicker, PowerMenuUI),
// same pattern as Notifications.qml's controlCenterVisible: each popup's
// LazyLoader watches its own property here, and a future keybind/IPC
// hookup (Phase 3 cutover, done manually) just needs to flip these.
Singleton {
    property bool launcherVisible: false
    property bool windowSwitcherVisible: false
    property bool clipboardVisible: false
    property bool cheatsheetVisible: false
    property bool themePickerVisible: false
    property bool powerMenuVisible: false
    property bool lockAppearanceVisible: false
    property bool emojiVisible: false

    function hideAll() {
        launcherVisible = false;
        windowSwitcherVisible = false;
        clipboardVisible = false;
        cheatsheetVisible = false;
        themePickerVisible = false;
        powerMenuVisible = false;
        lockAppearanceVisible = false;
        emojiVisible = false;
    }

    function toggleLauncher() { launcherVisible = !launcherVisible; }
    function toggleWindowSwitcher() { windowSwitcherVisible = !windowSwitcherVisible; }
    function toggleClipboard() { clipboardVisible = !clipboardVisible; }
    function toggleCheatsheet() { cheatsheetVisible = !cheatsheetVisible; }
    function toggleThemePicker() { themePickerVisible = !themePickerVisible; }
    function togglePowerMenu() { powerMenuVisible = !powerMenuVisible; }
    function toggleLockAppearance() { lockAppearanceVisible = !lockAppearanceVisible; }
    function toggleEmoji() { emojiVisible = !emojiVisible; }
}
