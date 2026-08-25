-- Autostart applications (run once on Hyprland start)
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

	-- Polkit agent, idle daemon, and lock screen are now handled inside
	-- quickshell itself (services/Polkit.qml, services/Idle.qml,
	-- services/Lock.qml) — hyprpolkitagent/hypridle/hyprlock stay installed
	-- as a manual fallback but are no longer autostarted, since only one
	-- polkit agent may be registered system-wide and two idle daemons would
	-- race each other.

	-- System tray / daemons (systemd-managed for auto-restart and logging)
	hl.exec_cmd("nm-applet --indicator &")
	hl.exec_cmd("systemctl --user start hyprpaper quickshell")

	-- Clipboard history
	hl.exec_cmd("systemctl --user start cliphist")

	-- Applications (staggered to avoid startup thundering herd)
	hl.exec_cmd("amazingmarvin --enable-wayland-ime")
	hl.exec_cmd("sleep 2 && bitwarden-desktop &")
	hl.exec_cmd("sleep 4 && nextcloud &")
	hl.exec_cmd("sleep 5 && /opt/teams-for-linux/teams-for-linux &")
	-- hl.exec_cmd("sleep 6 && zotero &")
end)
