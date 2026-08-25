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
stow zsh git nvim hypr quickshell systemd waybar swaync rofi tmux starship bat btop ghostty lazygit gpg opencode aerc mbsync herdr voxtype
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
| `herdr/` | [herdr](https://herdr.dev) terminal workspace manager for AI coding agents |
| `gpg/` | GPG agent config |
| `gtk/` | GTK theme config |
| `hypr/` | [Hyprland](https://hyprland.org/) compositor, hypridle, hyprlock |
| `kitty/` | [Kitty](https://sw.kovidgoyal.net/kitty/) terminal config |
| `lazygit/` | [lazygit](https://github.com/jesseduffield/lazygit) TUI |
| `mbsync/` | [mbsync/isync](https://isync.sourceforge.io/) email sync |
| `nvim/` | [Neovim](https://neovim.io/) config |
| `opencode/` | [opencode](https://opencode.ai/) AI coding assistant |
| `pacman/` | pacman config (parallel downloads etc.) |
| `quickshell/` | [Quickshell](https://quickshell.org/) — the active bar, notification daemon, launcher, OSDs, and lock screen. Replaced Waybar + SwayNC (see [Quickshell](#quickshell) below) |
| `rofi/` | [rofi](https://github.com/davatorium/rofi) launcher |
| `scripts/` | Install scripts and utilities |
| `sshs/` | [sshs](https://github.com/quantumsheep/sshs) SSH host manager |
| `starship/` | [Starship](https://starship.rs/) shell prompt |
| `swaync/` | [SwayNC](https://github.com/ErikReider/SwayNotificationCenter) notifications — **disabled**, superseded by Quickshell's own notification daemon; kept only as an unused config and a theme-template target |
| `theme/` | Not stowed — [Whiskers](https://github.com/catppuccin/whiskers)-based generator for hypr/waybar/ghostty/rofi/hyprlock/swaync/btop/lazygit/fzf/starship/neovim/emacs/quickshell/kitty theming and a single shell-wide font (+ matching wallpaper). See [Theming](#theming) below |
| `tmux/` | [tmux](https://github.com/tmux/tmux) terminal multiplexer |
| `voxtype/` | [Voxtype](https://github.com/peteonrails/voxtype) offline push-to-talk dictation |
| `vscode/` | VS Code / Cursor settings |
| `waybar/` | [Waybar](https://github.com/Alexays/Waybar) status bar — **disabled**, superseded by Quickshell's own bar; kept only as an unused config and a theme-template target |
| `wlogout/` | [wlogout](https://github.com/ArtsyMacaw/wlogout) logout menu |
| `zsh/` | Zsh config with oh-my-zsh |

## Quickshell

[Quickshell](https://quickshell.org/) is the active desktop shell — it replaced Waybar (bar) and SwayNC (notifications) in one cutover (`quickshell.service`, enabled; `waybar.service`/`swaync.service` disabled but left stowed as reference and as theme-template targets). Everything lives under `quickshell/.config/quickshell/`:

| Path | What's in it |
|------|--------------|
| `config/Config.qml` | Shell-wide layout knobs: bar height, font family (from the theme, see below), font weight, text-size scaling (`Config.px()`), privacy-indicator ignore list |
| `theme/Theme.qml` | Live-reloading color + font palette, backed by `theme/theme.json` (generated — see [Theming](#theming)) |
| `services/` | Backend singletons wrapping Hyprland IPC, Pipewire, Bluetooth, Networking, notifications, battery, brightness, MPRIS, etc. — one per concern, imported as `qs.services` |
| `modules/bar/` | The top bar: workspaces (pilled, animated focus indicator), active window title (marquee on overflow), tray widgets, quick panels (Network/Bluetooth/Tailscale/Weather/Battery/Display/Volume/Calendar/Vitals/Agents) |
| `modules/notifications/` | Toast popups + a control center (grouped-by-app history, quick actions, DND, Mpris card, per-app volume) |
| `modules/launcher/` | App launcher, window switcher, clipboard picker, cheatsheet, emoji picker, theme picker, power menu |
| `modules/osd/` | Volume/brightness on-screen displays |
| `modules/lock/`, `modules/polkit/` | Lock screen, polkit auth dialog |
| `modules/network/`, `modules/capture/` | Network/Bluetooth/Tailscale panels, screenshot capture menu |

Tailscale (status, up/down toggle, online-peer list with copy-IP) and Weather (current conditions + 3-day forecast, via [Open-Meteo](https://open-meteo.com/), no API key) are scoped-down ports of basecamp/omarchy's `shell/plugins/panels/tailscale` and `.../weather` plugins, adapted to this repo's own singleton+panel convention (`services/Tailscale.qml`/`services/Weather.qml` + their bar widgets/panels) instead of Omarchy's plugin framework — Weather replaced the old wttrbar-backed text-only pill.

Hyprland keybinds reach into the running shell via `quickshell ipc call <target> <function>` (see `services/Ipc.qml` for the full list of IPC targets — launcher, notifications, network, tailscale, weather, lock, idle, etc.).

**Conventions worth knowing before editing:**
- Every color comes from `Theme.qml` (`Theme.blue`, `Theme.surface1`, ...) — never a hardcoded hex/named color.
- Every font uses `Config.fontFamily`; every font size is wrapped in `Config.px(N)` so the Display panel's text-size slider scales it; every panel/popup window's `implicitWidth`/`implicitHeight` is also wrapped in `Config.px()` so panels grow with text size instead of overlapping.
- This machine's Hyprland build uses a Lua-native dispatch IPC (`hl.dsp.*` / `hl.monitor(...)`, see `hypr/.config/hypr/conf/keybindings.lua`) instead of vanilla Hyprland's `hyprctl dispatch <name> <args>` text protocol or `hyprctl keyword` — both are rejected outright on this build. Anything that shells out to `hyprctl` needs the Lua-table `eval` form instead.
- Changing monitor scale or moving workspaces between monitors doesn't emit a Hyprland event on its own — `services/Hypr.qml` explicitly refreshes monitors/workspaces after Hyprland events, and `services/MonitorScale.qml` refreshes again once its own scale-change command actually exits, since that path emits no event to hook a refresh off of.

## Theming

`theme/` (not stowed) is a [Whiskers](https://github.com/catppuccin/whiskers)-based generator that renders one shared palette + font into every themed app's own config format, so there's a single place to change a color or the font instead of editing N files.

- **Colors**: native Catppuccin flavors — `mocha`/`latte`/`frappe`/`macchiato` — plus custom palettes repainted via `--color-overrides` from `theme/palettes/*.json`: `nord`/`gruvbox`/`dracula`/`tokyonight`/`rosepine`/`everforest`/`kanagawa`/`matte-black`/`osaka-jade`.
- **Font**: a single `theme/font.json` (`{"family": "..."}`, currently `JetBrainsMono Nerd Font`) is the one place to change the shell-wide font. It's passed into every `theme/*.tera` template as a `font_family` override, and patched directly (via `jq`/`sed`) into the handful of apps that have no include/variable mechanism for it (VSCode, Zed, swaync's `style.css`, doom-emacs's `config.el`).
- **Targets covered**: Hyprland, hyprlock, Waybar, Ghostty, rofi, SwayNC, btop, lazygit, fzf, Starship, Neovim, Emacs, Quickshell, Kitty — plus the font-only patches above.
- Neovim and Emacs each load a self-generated colorscheme (`nvim/.config/nvim/colors/theme_generated.lua`, `doom/.doom.d/themes/theme-generated-theme.el`) picked up on next launch; Quickshell live-reloads `theme/theme.json` with no restart needed.

Apply a theme:

```sh
just -f theme/justfile apply <name>   # e.g. mocha, nord, dracula, ...
```

or press `SUPER+CTRL+SHIFT+SPACE` for a rofi picker with color-swatch previews (`theme/swatches/`, rebuild via `just -f theme/justfile swatches`) and a marker on the active theme.

To change only the font (keeping the current color theme), edit `theme/font.json` and re-run `apply` for whichever theme is currently active (`cat theme/.current`).

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
