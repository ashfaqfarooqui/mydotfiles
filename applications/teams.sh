cat ─╯ <<EOF >~/.local/share/applications/teams.desktop
[Desktop Entry]
Version=1.0
Name=Teams  
Comment=Teams  
Exec=google-chrome --app="https://teams.microsoft.com" --name=teams --class=teams  
Terminal=false
Type=Application
Icon=/home/$USER/mydotfiles/icons/teams.png  
Categories=GTK;
MimeType=text/html;text/xml;application/xhtml_xml;
StartupNotify=true
EOF
