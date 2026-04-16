# dotfiles

My personal dotfiles managed with [GNU Stow](https://www.gnu.org/software/stow/).

## Setup

### Prerequisites

Install `yay` (AUR helper) and `stow`:

```sh
# Install yay first (see scripts/install/installyay.sh)
yay -S stow --needed
```

### Install packages

```sh
bash scripts/install/setup.sh
```

### Stow configs

```sh
stow zsh git nvim hypr waybar swaync rofi tmux starship bat btop ghostty lazygit gpg opencode aerc mbsync
```

## Configs managed

| Directory | Description |
|-----------|-------------|
| `aerc/` | [aerc](https://aerc-mail.org/) TUI email client |
| `ashell/` | [ashell](https://github.com/MalpenZibo/ashell) status bar |
| `atuin/` | [atuin](https://github.com/atuinsh/atuin) shell history |
| `bat/` | [bat](https://github.com/sharkdp/bat) cat replacement |
| `btop/` | [btop](https://github.com/aristocratsoftware/btop) resource monitor |
| `chromimum/` | Chromium browser config |
| `doom/` | [Doom Emacs](https://github.com/doomemacs/doomemacs) config |
| `electron/` | Electron app flags (Wayland) |
| `fastfetch/` | [fastfetch](https://github.com/fastfetch-cli/fastfetch) system info |
| `ghostty/` | [Ghostty](https://ghostty.org/) terminal emulator |
| `git/` | Git config and aliases |
| `gpg/` | GPG agent config |
| `gtk/` | GTK theme config |
| `hypr/` | [Hyprland](https://hyprland.org/) compositor, hypridle, hyprlock |
| `kitty/` | [Kitty](https://sw.kovidgoyal.net/kitty/) terminal config |
| `lazygit/` | [lazygit](https://github.com/jesseduffield/lazygit) TUI |
| `mbsync/` | [mbsync/isync](https://isync.sourceforge.io/) email sync |
| `mechabar/` | Waybar theme |
| `nvim/` | [Neovim](https://neovim.io/) config |
| `opencode/` | [opencode](https://opencode.ai/) AI coding assistant |
| `pacman/` | pacman config (parallel downloads etc.) |
| `rofi/` | [rofi](https://github.com/davatorium/rofi) launcher |
| `scripts/` | Install scripts and utilities |
| `sshs/` | [sshs](https://github.com/quantumsheep/sshs) SSH host manager |
| `starship/` | [Starship](https://starship.rs/) shell prompt |
| `swaync/` | [SwayNC](https://github.com/ErikReider/SwayNotificationCenter) notifications |
| `tmux/` | [tmux](https://github.com/tmux/tmux) terminal multiplexer |
| `vscode/` | VS Code / Cursor settings |
| `walker/` | [walker](https://github.com/abenz1267/walker) launcher |
| `waybar/` | [Waybar](https://github.com/Alexays/Waybar) status bar |
| `wlogout/` | [wlogout](https://github.com/ArtsyMacaw/wlogout) logout menu |
| `zellij/` | [Zellij](https://zellij.dev/) terminal multiplexer |
| `zsh/` | Zsh config with oh-my-zsh |

## Zsh setup

Install [oh-my-zsh](https://ohmyz.sh/):

```sh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

Install plugins:

```sh
# Auto-suggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Syntax highlighting
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

# bat integration
git clone https://github.com/fdellwing/zsh-bat.git $ZSH_CUSTOM/plugins/zsh-bat

# powerlevel10k theme
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/themes/powerlevel10k
```

Then stow: `stow zsh`

Update the username in [zsh/.zshrc](zsh/.zshrc) if needed.

## fzf

```sh
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

## Rust tools (via rustup/cargo)

```sh
curl --proto '=https' --tlsv1.2 https://sh.rustup.rs | sh
cargo install --locked zellij
```

## Python (via uv)

```sh
curl -LsSf https://astral.sh/uv/install.sh | sh
```

## atuin

```sh
curl --proto '=https' --tlsv1.2 -LsSf https://setup.atuin.sh | sh
```

## tmux plugin manager

```sh
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

## gopass (password manager)

Dependencies:

```sh
pacman -S gnupg2 git
```

Zsh completions:

```sh
gopass completion zsh > ~/_gopass
sudo mv ~/_gopass /usr/share/zsh/site-functions/_gopass
rm -i ${ZDOTDIR:-${HOME:?No ZDOTDIR or HOME}}/.zcompdump && compinit
```

## GTK theme

```sh
# git clone https://github.com/Fausto-Korpsvart/Catppuccin-GTK-Theme
# cd theme && ./install.sh
```

## Network (Hyprland / WiFi)

To use `iwd` as the WiFi backend, create `/etc/NetworkManager/conf.d/wifi_backend.conf`:

```ini
[device]
wifi.backend=iwd
```

Then: `systemctl enable --now NetworkManager iwd` and disable `wpa_supplicant`.

## Mail (aerc + mbsync + davmail)

For Exchange/Office365 email setup, see [scripts/install/mail.sh](scripts/install/mail.sh).

## Fingerprint

See [scripts/install/setup-fingerprint.sh](scripts/install/setup-fingerprint.sh).

## Other notes

- `cups/` and `system/etc/krb5.conf` handle Chalmers printer setup.
- Ensure `~/.authinfo` exists with credentials for Emacs/mail tools.
- Scala: use [Coursier](https://www.scala-lang.org/2020/06/29/one-click-install.html) for Scala toolchain management.
- Package review: see [scripts/install/installs-review.md](scripts/install/installs-review.md) for a categorized list of historically installed packages to audit.
