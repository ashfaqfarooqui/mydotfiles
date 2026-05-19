-- Autostart applications (run once on Hyprland start)
hl.on("hyprland.start", function()
    -- XDG portal setup
    hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
    hl.exec_cmd("dbus-update-activation-environment --systemd --all")
    hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
    hl.exec_cmd("systemctl --user restart xdg-desktop-portal-hyprland xdg-desktop-portal")

    -- Polkit agent
    hl.exec_cmd("systemctl --user start hyprpolkitagent")

    -- System tray / daemons (systemd-managed for auto-restart and logging)
    hl.exec_cmd("nm-applet --indicator &")
    hl.exec_cmd("systemctl --user start hypridle hyprpaper swaync waybar")

    -- Clipboard history
    hl.exec_cmd("systemctl --user start cliphist")

    -- Applications (staggered to avoid startup thundering herd)
    hl.exec_cmd("amazingmarvin --enable-wayland-ime")
    hl.exec_cmd("sleep 2 && bitwarden-desktop &")
    hl.exec_cmd("sleep 4 && nextcloud &")
    hl.exec_cmd("sleep 5 && /opt/teams-for-linux/teams-for-linux &")
    hl.exec_cmd("sleep 6 && zotero &")
end)
