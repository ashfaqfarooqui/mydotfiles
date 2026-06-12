# ============================================================================
# .zshenv — environment variables for ALL zsh processes
# (interactive, non-interactive, login, scripts)
#
# Sourced first by zsh from $HOME/.zshenv (ZDOTDIR defaults to $HOME).
# Setting ZDOTDIR here redirects all subsequent config files
# (.zshrc, .zprofile, etc.) to ~/mydotfiles/zsh/.
# ============================================================================

# Tell zsh where its config lives (must be set here, before any other file)
export ZDOTDIR="$HOME/mydotfiles/zsh"

# --- XDG Base Directories ---
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# --- Editors ---
export EDITOR='zeditor'
export VISUAL='zeditor'
export ALTERNATE_EDITOR='nvim'

# --- GPG (needed by scripts and git commits, not just interactive shells) ---
export GPG_TTY=$(tty)

# --- PATH ---
# typeset -U prevents duplicate entries across nested shells / re-sources
typeset -U path
path=(
  $HOME/.tmuxifier/bin
  $HOME/opt/rocm/bin
  $path
)

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}/opt/rocm/lib:/opt/rocm/lib64"

# --- Toolchain env files ---
# Guarded to avoid double-sourcing when .profile was already run by the DM
[[ -z "$CARGO_ENV_LOADED" ]] && . "$HOME/.cargo/env"     && export CARGO_ENV_LOADED=1
[[ -z "$LOCAL_BIN_LOADED" ]] && . "$HOME/.local/bin/env" && export LOCAL_BIN_LOADED=1
[[ -z "$ATUIN_ENV_LOADED" ]] && . "$HOME/.atuin/bin/env" && export ATUIN_ENV_LOADED=1
