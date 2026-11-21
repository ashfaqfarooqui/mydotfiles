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
stow zsh git nvim hypr waybar swaync rofi wlogout tmux starship bat btop ghostty kitty lazygit gpg opencode aerc mbsync herdr-app voxtype \
     atuin chromium doom electron fastfetch gtk qt6ct sshs vscode zed
```

Stow the packages for apps that also write into their config folder with `--no-folding`:

```sh
stow --no-folding dms xdg nix direnv himalaya
```

The `networkmanager`, `pacman` and `systemd` packages hold files for `/etc`. Stow them as root with the target `/`:

```sh
sudo stow -t / networkmanager pacman systemd
```

Stow `claude` alone, with `--no-folding`, after `git-crypt unlock` (see the `claude/` row below):

```sh
git-crypt unlock
stow --no-folding claude
```

Stow `claude-archive` with `--no-folding`, because apps also write into `~/.local` and `~/.config/systemd/user`:

```sh
stow --no-folding claude-archive
```

## Configs managed

| Directory | Description |
|-----------|-------------|
| `aerc/` | [aerc](https://aerc-mail.org/) TUI email client |
| `atuin/` | [atuin](https://github.com/atuinsh/atuin) shell history |
| `bat/` | [bat](https://github.com/sharkdp/bat) cat replacement |
| `btop/` | [btop](https://github.com/aristocratsoftware/btop) resource monitor |
| `claude/` | [Claude Code](https://code.claude.com/docs) configuration: `settings.json`, `CLAUDE.md`, the status line script, `hooks/`, `commands/` and the user skills. `git-crypt` encrypts all files in this package. On a new machine, import the GPG key `7A804BCB51DE2D88` and run `git-crypt unlock` before you run `stow --no-folding claude`. If you do not unlock first, the links point to encrypted data and Claude Code shows a Settings Error. Use `--no-folding`, because Claude Code writes its data (`projects/`, `plans/`) into `~/.claude`. |
| `claude-archive/` | Daily job that keeps a copy of the Claude Code data off the machine: the script `claude-archive` and a systemd user timer. It compresses (`zstd`) and encrypts (`age`, public key only) the idle transcripts, the plans, the auto memory and `history.jsonl` into the git repo `~/claude-archive`, then pushes. It never deletes from the archive. After a good push it writes the time to `~/.local/state/claude-archive/last-push`. Test: `bash claude-archive/test/run-test.sh`. |
| `chromium/` | Chromium browser flags |
| `direnv/` | [direnv](https://direnv.net/) `direnvrc`: loads nix-direnv |
| `dms/` | [DankMaterialShell](https://danklinux.com/) settings and plugin lock. Targets `~/.config/dms` (`~/.config/DankMaterialShell` is a link to it). Installed plugins are not tracked |
| `doom/` | [Doom Emacs](https://github.com/doomemacs/doomemacs) config |
| `electron/` | Electron app flags (Wayland) |
| `fastfetch/` | [fastfetch](https://github.com/fastfetch-cli/fastfetch) system info |
| `ghostty/` | [Ghostty](https://ghostty.org/) terminal emulator |
| `git/` | Git config and aliases, global ignore file (`~/.config/git/ignore`) |
| `herdr-app/` | [Herdr](https://herdr.dev) app: keys, settings and the Catppuccin Mocha theme (the rest of `~/.config/herdr-app` is written by the app) |
| `gpg/` | GPG agent config |
| `gtk/` | GTK theme config |
| `hypr/` | [Hyprland](https://hyprland.org/) compositor (Lua config), hypridle, hyprlock. The desktop shell is DankMaterialShell (`dms/`, `conf/dms.lua`); set `use_dms = false` in `conf/autostart.lua` to fall back to the waybar stack. The git tag `pre-dms` has the old `hyprland.conf` setup |
| `himalaya/` | [himalaya](https://github.com/pimalaya/himalaya) CLI email client (password from `secret-tool`) |
| `kitty/` | [Kitty](https://sw.kovidgoyal.net/kitty/) terminal config |
| `lazygit/` | [lazygit](https://github.com/jesseduffield/lazygit) TUI |
| `mbsync/` | [mbsync/isync](https://isync.sourceforge.io/) email sync |
| `networkmanager/` | `/etc` drop-in: Wi-Fi power saving off |
| `nix/` | `nix.conf`: flakes and `nix-command` |
| `nvim/` | [Neovim](https://neovim.io/) config |
| `opencode/` | [opencode](https://opencode.ai/) AI coding assistant |
| `pacman/` | `/etc/pacman.conf` (parallel downloads etc.) |
| `qt6ct/` | Qt 6 theme config |
| `rofi/` | [rofi](https://github.com/davatorium/rofi) launcher |
| `scripts/` | Install scripts and utilities |
| `sshs/` | [sshs](https://github.com/quantumsheep/sshs) SSH host manager |
| `starship/` | [Starship](https://starship.rs/) shell prompt |
| `swaync/` | [SwayNC](https://github.com/ErikReider/SwayNotificationCenter) notifications |
| `systemd/` | `/etc` logind drop-in: lid and charger behaviour |
| `tmux/` | [tmux](https://github.com/tmux/tmux) terminal multiplexer |
| `voxtype/` | [Voxtype](https://github.com/peteonrails/voxtype) offline push-to-talk dictation |
| `vscode/` | VS Code / Cursor settings |
| `Wallpapers/` | Not stowed. Wallpaper images; `hyprpaper.conf` and `hyprlock.conf` point here |
| `waybar/` | [Waybar](https://github.com/Alexays/Waybar) status bar (fallback when DMS is off) |
| `wlogout/` | [wlogout](https://github.com/ArtsyMacaw/wlogout) logout menu |
| `xdg/` | `mimeapps.list`: default applications |
| `zed/` | [Zed](https://zed.dev/) editor settings and keymap |
| `zsh/` | Zsh config: plain zsh, Starship prompt, self-managed plugins (`conf/plugins.zsh`) |

## Zsh setup

```sh
bash zsh/setup.sh
```

The script checks the required tools, creates the XDG directories, stows `zsh` and sets zsh as the login shell. `conf/plugins.zsh` clones the plugins on the first shell start.

## fzf

```sh
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
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

- Ensure `~/.authinfo` exists with credentials for Emacs/mail tools.
- Scala: use [Coursier](https://www.scala-lang.org/2020/06/29/one-click-install.html) for Scala toolchain management.
- Retired configs (i3, xmonad, GNOME install scripts, etc.) are in the git tag `old-configs`.
