-- Autostart applications (run once on Hyprland start)

-- Desktop shell (bar, notifications, tray, idle, wallpaper, polkit agent).
-- On NixOS, home-manager writes nix/shell.lua for the chosen shell
-- (DankMaterialShell or waybar); it returns a table with start(). On Arch
-- there is no nix/shell.lua: Hyprland's require raises "module not found",
-- which pcall catches, and the waybar stack below starts as before.
local shell_ok, shell = pcall(require, "nix.shell")
-- Set to false to fall back to the waybar stack on Arch (git tag pre-dms has the old setup).
local use_dms = true
local start_shell = shell_ok and type(shell) == "table" and shell.start or nil

hl.on("hyprland.start", function()
	-- XDG portal setup
	hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
	hl.exec_cmd("dbus-update-activation-environment --systemd --all")
	hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
	hl.exec_cmd("systemctl --user restart xdg-desktop-portal-hyprland xdg-desktop-portal")

	-- xdg-desktop-portal-gtk's appearance/color-scheme setting comes from this
	-- gsetting (not from gtk-*.0/settings.ini), and Zen/Firefox's "Auto" theme
	-- reads it through the portal — without this, dark GTK themes don't
	-- actually signal "dark" to portal-aware apps.
	hl.exec_cmd("gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'")

	if start_shell then
		start_shell()
	elseif use_dms then
		-- DankMaterialShell: bar, notifications, launcher, lock, idle, wallpaper, clipboard, polkit
		hl.exec_cmd("dms run")
	else
		-- Polkit agent
		hl.exec_cmd("systemctl --user start hyprpolkitagent")

		-- System tray / daemons (systemd-managed for auto-restart and logging)
		hl.exec_cmd("nm-applet --indicator &")
		hl.exec_cmd("systemctl --user start hypridle hyprpaper swaync waybar")
	end

	-- Clipboard history (DMS has its own; cliphist only for the waybar fallback)
	if not use_dms then
		hl.exec_cmd("systemctl --user start cliphist")
	end

	-- Applications (staggered to avoid startup thundering herd)
	hl.exec_cmd("amazingmarvin --enable-wayland-ime")
	hl.exec_cmd("sleep 2 && bitwarden-desktop &")
	hl.exec_cmd("sleep 4 && nextcloud &")
	hl.exec_cmd("sleep 5 && teams-for-linux &")
	hl.exec_cmd("sleep 5 && firefoxpwa site launch 01KS07TJ055S08F7FCQPRS274A &")
	hl.exec_cmd("sleep 6 && zotero &")
end)
