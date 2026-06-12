#!/usr/bin/env bash
# ==============================================================================
# zsh/setup.sh — Bootstrap zsh config on a new system
#
# What this does:
#   1. Checks for required tools (and optionally installs missing ones on Arch)
#   2. Creates required XDG directories
#   3. Caches uv/uvx completions
#   4. Symlinks dotfiles via GNU Stow
#   5. Sets zsh as the default shell if it isn't already
#
# Plugins are self-managed by conf/plugins.zsh — they are cloned automatically
# on first shell start. No manual clone step needed here.
#
# Usage:
#   cd ~/mydotfiles
#   bash zsh/setup.sh
#
# Safe to re-run — everything is idempotent.
# ==============================================================================

set -euo pipefail

# ── Colours ───────────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
BLUE='\033[0;34m'; BOLD='\033[1m'; RESET='\033[0m'

info()    { echo -e "${BLUE}  --> ${RESET}$*"; }
success() { echo -e "${GREEN}  [ok] ${RESET}$*"; }
warn()    { echo -e "${YELLOW}  [!]  ${RESET}$*"; }
error()   { echo -e "${RED}  [!!] ${RESET}$*" >&2; }
header()  { echo -e "\n${BOLD}${BLUE}==>${RESET}${BOLD} $*${RESET}"; }

# ── Config ────────────────────────────────────────────────────────────────────
DOTFILES_DIR="${DOTFILES_DIR:-$HOME/mydotfiles}"

# Required tools — script will warn if missing and offer Arch install
REQUIRED_TOOLS=(zsh git curl stow)

# Tools used at runtime by the zsh config (warn only, don't block setup)
RUNTIME_TOOLS=(fzf bat eza zoxide atuin starship trash-put dust btm duf procs kitten uv)

# ── Helpers ───────────────────────────────────────────────────────────────────
command_exists() { command -v "$1" &>/dev/null; }

install_arch() {
  local pkg=$1
  if command_exists yay; then
    yay -S --noconfirm "$pkg"
  elif command_exists paru; then
    paru -S --noconfirm "$pkg"
  else
    sudo pacman -S --noconfirm "$pkg"
  fi
}

# ── Step 1: Check required tools ──────────────────────────────────────────────
header "Checking required tools"

missing_required=()
for tool in "${REQUIRED_TOOLS[@]}"; do
  if command_exists "$tool"; then
    success "$tool"
  else
    error "$tool not found"
    missing_required+=("$tool")
  fi
done

if [[ ${#missing_required[@]} -gt 0 ]]; then
  warn "Missing required tools: ${missing_required[*]}"
  if command_exists pacman; then
    read -rp "  Install them via pacman/yay? [y/N] " ans
    if [[ "${ans,,}" == "y" ]]; then
      for tool in "${missing_required[@]}"; do
        info "Installing $tool..."
        install_arch "$tool"
      done
    else
      error "Cannot continue without: ${missing_required[*]}"
      exit 1
    fi
  else
    error "Please install the missing tools manually and re-run."
    exit 1
  fi
fi

# Warn about missing runtime tools (non-fatal)
header "Checking runtime tools"
missing_runtime=()
for tool in "${RUNTIME_TOOLS[@]}"; do
  if command_exists "$tool"; then
    success "$tool"
  else
    warn "$tool not found — some features will not work"
    missing_runtime+=("$tool")
  fi
done

if [[ ${#missing_runtime[@]} -gt 0 ]]; then
  echo ""
  warn "Missing runtime tools: ${missing_runtime[*]}"
  warn "Install them to get full functionality. Continuing anyway."
fi

# ── Step 2: Create XDG directories ────────────────────────────────────────────
header "Creating XDG directories"

dirs=(
  "${XDG_STATE_HOME:-$HOME/.local/state}/zsh"
  "${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
  "${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"
  "${XDG_DATA_HOME:-$HOME/.local/share}/zsh/completions"
  "$HOME/.zsh/completions"
  "$HOME/.zfunc"
)

for d in "${dirs[@]}"; do
  if [[ -d "$d" ]]; then
    success "$d (exists)"
  else
    mkdir -p "$d"
    success "$d (created)"
  fi
done

# ── Step 3: Cache uv/uvx completions ─────────────────────────────────────────
header "Caching uv/uvx completions"

uv_cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/uv-completions.zsh"
if command_exists uv; then
  uv generate-shell-completion zsh >  "$uv_cache"
  if command_exists uvx; then
    uvx --generate-shell-completion zsh >> "$uv_cache"
  fi
  success "uv completions cached → $uv_cache"
else
  warn "uv not found — skipping (will be generated on first shell start when uv is available)"
fi

# ── Step 4: Stow dotfiles ─────────────────────────────────────────────────────
header "Stowing dotfiles"

if [[ ! -d "$DOTFILES_DIR" ]]; then
  error "Dotfiles directory not found: $DOTFILES_DIR"
  error "Clone your dotfiles repo first, or set DOTFILES_DIR=/path/to/repo"
  exit 1
fi

# Back up any pre-existing non-symlink files that stow would conflict with
for f in .zshenv .zshrc .zprofile; do
  target="$HOME/$f"
  if [[ -f "$target" && ! -L "$target" ]]; then
    backup="$target.pre-stow-backup"
    warn "$target is a plain file — backing up to $backup"
    mv "$target" "$backup"
  fi
done

stow --dir="$DOTFILES_DIR" --target="$HOME" --restow zsh
success "Stow applied — symlinks created"

# Verify the critical symlink
if [[ -L "$HOME/.zshenv" ]]; then
  resolved=$(readlink -f "$HOME/.zshenv")
  success ".zshenv → $resolved"
else
  error ".zshenv was not symlinked — check stow output above"
fi

# ── Step 5: Set default shell ─────────────────────────────────────────────────
header "Default shell"

zsh_path=$(command -v zsh)
current_shell=$(getent passwd "$USER" | cut -d: -f7)

if [[ "$current_shell" == "$zsh_path" ]]; then
  success "zsh is already the default shell ($zsh_path)"
else
  info "Changing default shell to $zsh_path"
  if grep -qF "$zsh_path" /etc/shells; then
    chsh -s "$zsh_path"
    success "Default shell changed — log out and back in for it to take effect"
  else
    warn "$zsh_path is not in /etc/shells — adding it"
    echo "$zsh_path" | sudo tee -a /etc/shells
    chsh -s "$zsh_path"
    success "Default shell changed"
  fi
fi

# ── Done ──────────────────────────────────────────────────────────────────────
echo ""
echo -e "${BOLD}${GREEN}Done!${RESET}"
echo ""
echo "  Next steps:"
echo "  1. Open a new terminal (or: exec zsh)"
echo "     Plugins will self-install on first start."
echo "  2. Verify with: echo \$ZDOTDIR"
echo "     Expected:    $HOME/mydotfiles/zsh"
echo ""
echo "  To update plugins later, run inside zsh:"
echo "    plugin-update"
echo ""
echo "  To refresh uv completions after upgrading uv:"
echo "    rm ${XDG_CACHE_HOME:-$HOME/.cache}/zsh/uv-completions.zsh && exec zsh"
echo ""
