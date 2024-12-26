chsh -s $(which zsh)

cd /tmp
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
