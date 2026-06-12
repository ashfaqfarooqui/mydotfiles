# ============================================================================
# Plugins
#
# Self-bootstrapping loader — clones a plugin on first use, sources it every
# time. No separate install step needed on a new machine.
#
# _plugin_load  <github-user> <repo> [entry-file]
#   entry-file defaults to <repo>.plugin.zsh
#
# _plugin_vendor <name> <raw-url>
#   Downloads a single raw file (for plugins that aren't a full git repo).
#
# plugin-update
#   Pull latest for every cloned plugin.
# ============================================================================

ZPLUGINDIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"

_plugin_load() {
  local user="$1" repo="$2" entry="${3:-${2}.plugin.zsh}"
  local plugin_path="${ZPLUGINDIR}/${repo}"

  if [[ ! -d "$plugin_path" ]]; then
    echo "Installing ${repo}..."
    git clone --depth=1 "https://github.com/${user}/${repo}" "$plugin_path" \
      || { echo "ERROR: failed to install ${repo}" >&2; return 1; }
  fi

  source "${plugin_path}/${entry}"
}

_plugin_vendor() {
  local name="$1" url="$2"
  local dest="${ZPLUGINDIR}/${name}/${name}.plugin.zsh"

  if [[ ! -f "$dest" ]]; then
    echo "Fetching ${name}..."
    mkdir -p "${ZPLUGINDIR}/${name}"
    curl -fsSL "$url" -o "$dest" \
      || { echo "ERROR: failed to fetch ${name}" >&2; return 1; }
  fi

  source "$dest"
}

plugin-update() {
  local dir
  for dir in "${ZPLUGINDIR}"/*/; do
    if [[ -d "${dir}.git" ]]; then
      echo "Updating ${dir:t}..."
      git -C "$dir" pull --ff-only
    fi
  done
}

# --- Load plugins (order matters) ---

# fzf-powered tab completion
_plugin_load Aloxaf fzf-tab

# Universal archive extraction (non-standard entry file)
_plugin_load xvoland Extract extract.sh

# ESC ESC — toggle sudo (vendored single file, not a full repo clone)
_plugin_vendor sudo \
  "https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/sudo/sudo.plugin.zsh"

# Fish-style inline autosuggestions
_plugin_load zsh-users zsh-autosuggestions

# Remind you to use an alias when you type its full expansion
_plugin_load MichaelAquilina zsh-you-should-use

# fzf shell integration — keybindings (Ctrl-T, Alt-C) and completion
# Must come BEFORE atuin so atuin can overwrite Ctrl-R
eval "$(fzf --zsh)"

# Atuin — history search, replaces Ctrl-R (must be after fzf to win the binding)
eval "$(atuin init zsh)"

# Fast syntax highlighting — drop-in replacement for zsh-syntax-highlighting, MUST be last
_plugin_load zdharma-continuum fast-syntax-highlighting
