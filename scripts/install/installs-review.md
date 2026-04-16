# Install History Review

Review each package below. Mark as **KEEP** (already in setup.sh or want as default) or **REMOVE** (no longer needed).
Packages already covered by `setup.sh` are noted.

---

## Shell & Terminal

| Package | Notes | Decision |
|---------|-------|----------|
| zsh | core shell | KEEP (in setup.sh) |
| bat | cat replacement | KEEP (in setup.sh) |
| btop | resource monitor | KEEP (in setup.sh) |
| fzf | fuzzy finder | KEEP (in setup.sh) |
| zoxide | smart cd | KEEP (in setup.sh) |
| eza | ls replacement | KEEP (in setup.sh) |
| fd | find replacement | KEEP (in setup.sh) |
| dust | du replacement | KEEP (in setup.sh) |
| duf | df replacement | KEEP (in setup.sh) |
| ripgrep | fast grep | KEEP |
| git-delta | better git diff | KEEP (in setup.sh) |
| bottom | system monitor (btm) | KEEP (in setup.sh) |
| broot | interactive tree | KEEP (in setup.sh) |
| sd | sed replacement | KEEP (in setup.sh) |
| hyperfine | benchmarking | KEEP (in setup.sh) |
| bandwhich | network utilization | KEEP (in setup.sh) |
| tokei | code stats | KEEP (in setup.sh) |
| trash-cli | safer rm | KEEP (in setup.sh) |
| tree | directory tree | KEEP |
| cloc | count lines of code | REMOVE? |
| nmap | network scanner | REMOVE? |
| net-tools | ifconfig etc | KEEP (in setup.sh) |
| gnu-netcat | netcat | REMOVE? |
| inetutils | network utils (hostname etc) | REMOVE? |
| nethogs | per-process network | REMOVE? |
| iftop | network bandwidth | REMOVE? |
| whois | domain lookup | REMOVE? |
| strace | system call tracer | REMOVE? |
| bind / nslookup | DNS tools | REMOVE? |

## Editors & Dev Tools

| Package | Notes | Decision |
|---------|-------|----------|
| neovim | main editor | KEEP (in setup.sh) |
| vscode | GUI editor | KEEP (in setup.sh) |
| emacs / emacs-gcc-wayland-devel-bin | Doom Emacs | KEEP? |
| opencode-bin | AI coding tool | KEEP (in setup.sh) |
| lazygit | git TUI | KEEP (in setup.sh) |
| git | version control | KEEP (in setup.sh) |
| yq | YAML processor | KEEP (in setup.sh) |
| jq | JSON processor | KEEP (in setup.sh - as `jg`) |
| pandoc | doc converter | REMOVE? |
| aspell | spell checker | KEEP |
| ispell | spell checker | KEEP |
| tree-sitter-cli | parser toolkit | KEEP (in setup.sh) |
| rust-analyzer | Rust LSP | KEEP |
| pyright | Python LSP | REMOVE? |
| python-poetry | Python env manager | REMOVE? |
| uv | Python package manager | KEEP (in setup.sh) |
| sbt | Scala build tool | REMOVE? |
| leiningen | Clojure build tool | REMOVE? |
| opam | OCaml package manager | REMOVE? |
| just | command runner | KEEP? |
| mkcert | local TLS certs | REMOVE? |
| doxygen | doc generator | REMOVE? |
| cppcheck | C++ static analysis | REMOVE? |
| graphviz / dot | graph rendering | REMOVE? |
| protobuf / protoc | Protocol Buffers | REMOVE? |
| luarocks | Lua package manager | REMOVE? |
| lua | Lua interpreter | REMOVE? |
| cargo-nextest | Rust test runner | KEEP |
| cargo-tarpaulin | Rust coverage | KEEP |

## Hyprland & Desktop

| Package | Notes | Decision |
|---------|-------|----------|
| hyprpaper | wallpaper | KEEP (in setup.sh) |
| hyprlock | screen lock | KEEP (in setup.sh) |
| hypridle | idle daemon | KEEP (in setup.sh) |
| hyprshot | screenshot | KEEP (in setup.sh) |
| hyprpicker | color picker | KEEP (in setup.sh) |
| hyprpolkitagent | auth agent | KEEP (in setup.sh) |
| hyprsunset | blue light filter | KEEP (in setup.sh) |
| hyprshade | shader effects | KEEP (in setup.sh) |
| hyprcursor | cursor theme | keep |
| swaync | notifications | KEEP (in setup.sh) |
| waybar | status bar | KEEP (in setup.sh) |
| wlogout | logout menu | KEEP (in setup.sh) |
| rofi | launcher | KEEP (in setup.sh) |
| nwg-look | GTK theme switcher | KEEP (in setup.sh) |
| xdg-desktop-portal-hyperland | portal | KEEP (in setup.sh) |
| xdg-desktop-portal-wlr | portal (wlroots) | REMOVE? |
| xdg-desktop-portal-gtk | portal (GTK) | REMOVE? |
| xwayland / xorg-xwayland | XWayland compat | KEEP? |
| xorg-xhost | X host auth | REMOVE? |
| cliphist | clipboard manager | KEEP (in setup.sh) |
| xclip | clipboard CLI | KEEP (in setup.sh) |
| grim + slurp | screenshot tools | KEEP |
| xwaylandvideobridge | screen sharing | REMOVE? (commented out) |
| brightnessctl | brightness control | KEEP (in setup.sh) |
| network-manager-applet | NM tray | KEEP (in setup.sh) |
| blueman | bluetooth manager | KEEP (in setup.sh) |
| bluetui | bluetooth TUI | KEEP (in setup.sh) |
| gnome-keyring | keyring | KEEP (in setup.sh) |
| libsecret | secret storage | KEEP (in setup.sh) |
| libnotify | desktop notifications | KEEP (in setup.sh) |
| sddm-silent-theme | login screen theme | KEEP (in setup.sh) |
| timeshift | system snapshots | KEEP (in setup.sh) |
| evtest | input event tester | REMOVE? |

## Fonts

| Package | Notes | Decision |
|---------|-------|----------|
| ttf-jetbrains-mono-nerd | main coding font | KEEP (in setup.sh) |
| otf-font-awesome | icon font | KEEP (in setup.sh) |
| ttf-fira-code | coding font | KEEP (in setup.sh) |
| ttf-firacode-nerd | nerd font variant | KEEP (in setup.sh) |
| ttf-nerd-fonts-symbols | nerd symbols | KEEP (in setup.sh) |
| ttf-aptos | Aptos font | KEEP (in setup.sh) |
| noto-fonts | unicode coverage | KEEP (in setup.sh) |
| ttf-twemoji | emoji font | REMOVE? |
| ibm-plex-mono / ttf-ibm-plex-mono | IBM Plex | REMOVE? |
| nerd-fonts-meslo | Meslo nerd font | REMOVE? |
| nerd-fonts-overpass | Overpass nerd font | REMOVE? |

## Applications

| Package | Notes | Decision |
|---------|-------|----------|
| obsidian | notes | KEEP (in setup.sh) |
| zotero-bin | reference manager | KEEP (in setup.sh) |
| calibre | ebook manager | KEEP (in setup.sh) |
| chromium | browser | KEEP (in setup.sh) |
| zen-browser-bin | browser | KEEP (in setup.sh) |
| bitwarden | password manager | KEEP (in setup.sh) |
| bitwarden-cli | password manager CLI | KEEP (in setup.sh) |
| rofi-rbw | rofi bitwarden | KEEP (in setup.sh) |
| teams-for-linux | MS Teams | KEEP (in setup.sh) |
| zoom | video calls | REMOVE |
| discord | chat | REMOVE |
| amazingmarvin-appimage | task manager | KEEP (in setup.sh) |
| morgen | calendar | REMOVE |
| libreoffice-fresh | office suite | KEEP (in setup.sh) |
| evince | PDF viewer | KEEP (in setup.sh) |
| gparted | partition editor | KEEP (in setup.sh) |
| inkscape | vector graphics | REMOVE? |
| rawtherapee | photo editor | KEEP |
| shotwell | photo manager | REMOVE? |
| vlc | media player | REMOVE? |
| mpv | media player | KEEp |
| obs-studio | screen recorder | REMOVE |
| ghostty | terminal emulator | KEEP(add to setup) |
| thunderbird | email client | REMOVE (using aerc) |
| nextcloud-client | cloud sync | KEEP |
| remmina | remote desktop | KEEP (in setup.sh) |
| freerdp | RDP client | KEEP (in setup.sh) |
| ollama | local LLM runner | REMOVE |
| docker | containers | KEEP (in setup.sh) |
| docker-compose | compose | KEEP (in setup.sh) |
| lazydocker | docker TUI | KEEP (in setup.sh) |
| portainer | docker GUI | KEEP (in setup.sh) |
| docker-buildx | buildx plugin | REMOVE? |
| solaar | Logitech devices | KEEP (in setup.sh) |
| fprintd | fingerprint auth | KEEP (in setup.sh) |
| localsend-bin | local file sharing | KEEP |
| sshs | SSH host manager | KEEP |
| tailscale | VPN mesh | KEEP |
| wireguard-tools | WireGuard VPN | KEEP (in setup.sh) |
| expressvpn | commercial VPN | REMOVE |
| davmail | Exchange gateway | REMOVE |
| isync / mbsync | email sync | KEEP (in setup.sh) |
| aerc | TUI email client | KEEP (in setup.sh) |
| logseq-desktop | note-taking | REMOVE  |
| joplin | note-taking | REMOVE |

## Power & Hardware

| Package | Notes | Decision |
|---------|-------|----------|
| tlp | laptop power mgmt | KEEP (in setup.sh) |
| tlp-rdw | radio mgmt | KEEP (in setup.sh) |
| tlpui | TLP GUI | KEEP (in setup.sh) |
| auto-cpufreq | CPU freq scaling | KEEP (in setup.sh) |
| amdgpu_top | AMD GPU monitor | KEEP (in setup.sh) |
| rocm-smi | ROCm system mgmt | KEEP |
| rocm-core + rocm-hip-sdk | ROCm compute | REMOVE? |
| fwupd | firmware updater | KEEP (add to setup) |
| amd-ucode | AMD microcode | KEEP |
| vulkan-radeon | AMD Vulkan driver | KEEP |
| mesa | OpenGL | REMOVE? |
| libdrm | DRM library | REMOVE? |
| sensors | hardware sensors | KEEP |
| ntfs-3g | NTFS support | KEEP (in setup.sh) |

## LaTeX

| Package | Notes | Decision |
|---------|-------|----------|
| texlive / texlive-latex | LaTeX | REMOVE? |
| texlive-latexrecommended | LaTeX extras | REMOVE? |
| texlive-latexextra | LaTeX extras | REMOVE? |
| latexmk | LaTeX build tool | REMOVE? |

## Formal Methods / Research Tools

| Package | Notes | Decision |
|---------|-------|----------|
| tlatoolbox | TLA+ toolbox | REMOVE? |
| spin / ispin | PROMELA model checker | REMOVE? |
| nusmv | NuSMV model checker | REMOVE? |
| minizinc | constraint solver | REMOVE? |
| z3-solver (pip) | SMT solver | REMOVE? |
| libz3 | Z3 library | REMOVE? |

## npm globals

| Package | Notes | Decision |
|---------|-------|----------|
| yo + generator-office | Office add-in scaffold | REMOVE? |
| mldoc | Markdown/Org converter | REMOVE? |

## pip / uv installs

| Package | Notes | Decision |
|---------|-------|----------|
| youtube-dl | YouTube downloader | REMOVE |
| carla | CARLA simulator | REMOVE |
| tensorflow-rocm | TF with ROCm | REMOVE |

---

> **How to use:** Go through each table, change REMOVE? to REMOVE and KEEP? to KEEP.
> Then move confirmed KEEP packages that aren't already in `setup.sh` into the appropriate section.
