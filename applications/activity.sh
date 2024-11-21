cat <<EOF >~/.local/share/applications/Activity.desktop
[Desktop Entry]
Version=1.0
Name=Activity
Comment=System activity from btop
Exec=kitty --class=Kitty --title=Activity -e btop
Terminal=false
Type=Application
Icon=/home/$USER/mydotfiles/icons/Activity.png
Categories=GTK;
StartupNotify=false
EOF
