# zsh

Zsh with [[https://ohmyz.sh/][oh-my-zsh]] has been my shell for a long time now.

Install zsh, I guess this is already done at this stage by [[*Installing all programs][Installing all programs]].
# +BEGIN_SRC sh
yay -S zsh
chsh -s $(which zsh)
# +END_SRC

Install oh-my-zsh
# +BEGIN_SRC sh
sh -c "$(curl -fsSL <https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh>)"
# +END_SRC

Install additional plugins I require:

- auto-suggestions
# +BEGIN_SRC sh
git clone <https://github.com/zsh-users/zsh-autosuggestions> ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# +END_SRC
- powerlevel10k theme
# +BEGIN_SRC sh
git clone --depth=1 <https://github.com/romkatv/powerlevel10k.git> ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/themes/powerlevel10k
# +END_SRC
- Syntax highlighting
# +BEGIN_SRC sh
git clone <https://github.com/zsh-users/zsh-syntax-highlighting.git> ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

# +END_SRC

- cat Bat
  #+begin_src sh
git clone <https://github.com/fdellwing/zsh-bat.git> $ZSH_CUSTOM/plugins/zsh-bat
  #+end_src
Stow the config files:
# +BEGIN_SRC sh
stow zsh
# +END_SRC
Additionally, make sure to update the user name in the [[file:zsh/.zshrc][.zshrc]] file to the correct one.

- Fzf

It might be the case that [[https://github.com/junegunn/fzf][fzf]] needs to be installed separately.

# +BEGIN_SRC sh
git clone --depth 1 <https://github.com/junegunn/fzf.git> ~/.fzf
~/.fzf/install
# +END_SRC
- Some additional notes:
The folder [[file:scripts/][scripts]] has some scripts mainly used by the i3 config.

configurations for [[file:cups/][cups]] and [[file:system/etc/krb5.conf][kbr5]] are meant to get printers working at Chalmers.

Ensure to have .authinfo file in the home directory with all the required credential's for things to work. It might also be needed to be added to the Emacs.d folder created by Doom.
- TODO scala installation
The scala community has come out with a new tool to install and maintain scala versions. May I need to look into that one for a more stable experience with Scala toolchain.
<https://www.scala-lang.org/2020/06/29/one-click-install.html>

- Gopass
dependencies
# +begin_src shell
pacman -S gnupg2 git rng-tools
# +end_src
install zsh auto completions
# +begin_src shell
 gopass completion zsh > ~/_gopass
 sudo mv ~/_gopass /usr/share/zsh/site-functions/_gopass
 rm -i ${ZDOTDIR:-${HOME:?No ZDOTDIR or HOME}}/.zcompdump && compinit

# +end_src

frontend
# +begin_src shell
pacman -S qtpass
# +end_src

- Portal

> curl -sL portal.spatiumportae.com | bash
>

- SShs

> <https://github.com/quantumsheep/sshs/releases>
>
>

- TMUx


# Zoxide

curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | sh


# sesh


