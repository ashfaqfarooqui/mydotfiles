#!/usr/bin/env bash
# ==============================================================================
# scripts/install/changeShell.sh
#
# Sets up zsh as the default shell and bootstraps the zsh config.
# Plugin management, ZDOTDIR, XDG dirs, and stow are all handled by
# zsh/setup.sh — this script is a thin entry point.
#
# Usage:
#   cd ~/mydotfiles
#   bash scripts/install/changeShell.sh
# ==============================================================================

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# ── Install starship if missing ───────────────────────────────────────────────
# On Arch this is handled by setup.sh (yay -S starship).
# This curl fallback covers other distros or a manual run before setup.sh.
if ! command -v starship &>/dev/null; then
  echo "--> Installing starship..."
  curl -fsSL https://starship.rs/install.sh | sh
fi

# ── Delegate to zsh/setup.sh ──────────────────────────────────────────────────
# Handles: XDG dirs, plugin bootstrap, stow, chsh
DOTFILES_DIR="$DOTFILES_DIR" bash "$DOTFILES_DIR/zsh/setup.sh"
