# ============================================================================
# .zshrc — interactive shell orchestrator
# Read via ZDOTDIR (set in ~/.zshenv → ~/mydotfiles/zsh)
# ============================================================================

# --- Shell options ---
setopt correct              # suggest corrections for mistyped commands
setopt auto_cd              # type a directory name to cd into it
setopt hist_ignore_dups     # don't store consecutive duplicate entries
setopt share_history        # share history across all zsh sessions
setopt extended_history     # record timestamp with each entry
setopt hist_ignore_space    # prefix command with space to keep it out of history
setopt no_beep              # silence all terminal bells
setopt numeric_glob_sort    # sort filenames numerically when applicable

# --- History ---
# Atuin owns history search (Ctrl-R), but zsh still needs HISTFILE for !! and !$
HISTSIZE=10000
SAVEHIST=10000
HISTFILE="${XDG_STATE_HOME}/zsh/history"

# --- Completions ---
# XDG-compliant fpath entries
fpath=(~/.zsh/completions ~/.zfunc "${XDG_DATA_HOME}/zsh/completions" $fpath)

# Single compinit call — dump cached to XDG cache dir
autoload -Uz compinit
compinit -d "${XDG_CACHE_HOME}/zsh/zcompdump"

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z-_}={A-Z_-}'  # hyphen-insensitive

# --- SSH auth (Bitwarden agent) ---
export SSH_AUTH_SOCK="$HOME/.bitwarden-ssh-agent.sock"

# --- Load modular config ---
source "$ZDOTDIR/conf/plugins.zsh"
source "$ZDOTDIR/conf/aliases.zsh"
source "$ZDOTDIR/conf/fzf.zsh"
source "$ZDOTDIR/conf/prompt.zsh"

# ============================================================================
# Tool completions
# ============================================================================

# uv / uvx — cached to avoid regenerating on every shell start
# Delete the cache file to force a refresh after upgrading uv
_uv_comp_cache="${XDG_CACHE_HOME}/zsh/uv-completions.zsh"
if [[ ! -f "$_uv_comp_cache" ]]; then
  uv generate-shell-completion zsh >  "$_uv_comp_cache"
  uvx --generate-shell-completion zsh >> "$_uv_comp_cache"
fi
source "$_uv_comp_cache"

# entire CLI
source <(entire completion zsh)

# opencode (yargs-based)
#compdef opencode
###-begin-opencode-completions-###
_opencode_yargs_completions()
{
  local reply
  local si=$IFS
  IFS=$'
' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" opencode --get-yargs-completions "${words[@]}"))
  IFS=$si
  if [[ ${#reply} -gt 0 ]]; then
    _describe 'values' reply
  else
    _default
  fi
}
if [[ "${zsh_eval_context[-1]}" == "loadautofunc" ]]; then
  _opencode_yargs_completions "$@"
else
  compdef _opencode_yargs_completions opencode
fi
###-end-opencode-completions-###

# ============================================================================
# Functions
# ============================================================================

# Fuzzy-find a file and open it in $EDITOR
ef() { fzf | xargs -r -I % $EDITOR % ;}

# Fuzzy-find a dotfile and open it in $EDITOR
ec() { du -a ~/mydotfiles/* | awk '{print $2}' | fzf | xargs -r $EDITOR ;}

# --- Git worktree helpers ---

# Switch to a worktree by branch name
swtree() {
  local dir
  dir=$(git worktree list | grep "$1" | awk '{print $1}')
  if [[ -d "$dir" ]]; then
    cd "$dir"
  else
    echo "Worktree '$1' not found"
  fi
}

# Create a new worktree: newtree <directory> <branch>
newtree() {
  if [[ $# -ne 2 ]]; then
    echo "Usage: newtree <directory> <branch>"
    return 1
  fi
  git worktree add "$1" "$2"
}

# Remove a worktree: rmtree <directory>
rmtree() {
  if [[ $# -ne 1 ]]; then
    echo "Usage: rmtree <directory>"
    return 1
  fi
  git worktree remove "$1"
}

# Show git status in every worktree
worktree-status() {
  for dir in $(git worktree list | awk '{print $1}' | tail -n +2); do
    echo "Checking status in: $dir"
    (cd "$dir" && git status --short)
  done
}

# ============================================================================
# Misc
# ============================================================================

OPENCODE_EXPERIMENTAL=true
