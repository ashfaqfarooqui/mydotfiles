-- DMS keybind reconciliation. Runs after dms/binds.lua and before conf/keybindings.lua.
-- hl.unbind removes every earlier registration of an exact-case key string (Hyprland
-- docs: Configuring > Core > Binds > Unbind), so the DMS defaults that overlap with
-- conf/keybindings.lua are dropped here and the remaining DMS binds are kept.

-- Window/workspace/monitor binds: conf/keybindings.lua is authoritative.
for _, key in ipairs({
	"SUPER + H", "SUPER + J", "SUPER + K", "SUPER + L",
	"SUPER + SHIFT + H", "SUPER + SHIFT + J", "SUPER + SHIFT + K", "SUPER + SHIFT + L",
	"SUPER + CTRL + L", "SUPER + R", "SUPER + O", "SUPER + M", "SUPER + Q", "SUPER + F",
	"SUPER + N", "SUPER + SHIFT + N", "SUPER + comma", "SUPER + P", "SUPER + SHIFT + P",
	"SUPER + mouse:272", "SUPER + mouse:273", "SUPER + mouse_down", "SUPER + mouse_up",
}) do
	hl.unbind(key)
end

-- DMS features relocated onto SUPER + ALT so they don't shadow the binds above.
hl.bind("SUPER + ALT + N", hl.dsp.exec_cmd("dms ipc call notifications toggle"))
hl.bind("SUPER + ALT + SHIFT + N", hl.dsp.exec_cmd("dms ipc call notepad toggle"))
hl.bind("SUPER + ALT + comma", hl.dsp.exec_cmd("dms ipc call settings focusOrToggle"))
hl.bind("SUPER + ALT + P", hl.dsp.exec_cmd("dms ipc outputs cycleProfile"))
hl.bind("SUPER + ALT + SHIFT + P", hl.dsp.dpms({ action = "toggle" }))
