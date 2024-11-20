cat ─╯ <<EOF >~/.local/share/applications/Reader.desktop
[Desktop Entry]
Version=1.0
Name=Teams  
Comment=Teams  
Exec=google-chrome --app="https://read.readwise.io" --name=Reader --class=reader  
Terminal=false
Type=Application
Icon=/home/$USER/mydotfiles/icons/teams.png  
Categories=GTK;
MimeType=text/html;text/xml;application/xhtml_xml;
StartupNotify=true
EOF
