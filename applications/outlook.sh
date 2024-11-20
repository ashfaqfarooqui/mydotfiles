cat ─╯ <<EOF >~/.local/share/applications/Outlook.desktop
[Desktop Entry]
Version=1.0
Name=Outlook 
Comment=Outlook           
Exec=google-chrome --app="https://outlook.office.com" --name=outlook --class=Outlook
Terminal=false
Type=Application
Icon=/home/$USER/mydotfiles/icons/outlook.png                       
Categories=GTK;
MimeType=text/html;text/xml;application/xhtml_xml;
StartupNotify=true
EOF
