# ============================================================================
# Aliases
# ============================================================================

# --- Navigation ---
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias cd='z'

# --- Editors ---
alias v='nvim'
alias vim='nvim'
alias vi='nvim'
alias e='emacsclient -c -a emacs'
alias et='emacsclient -t'
alias zed='zeditor'
alias nvim-kickstart='NVIM_APPNAME="nvim-kickstart" nvim'
alias nvim-personal='NVIM_APPNAME="nvim-personal" nvim'
alias zshconfig="$EDITOR $ZDOTDIR/.zshrc"

# --- SSH ---
alias s='kitten ssh'

# --- Music ---
alias music='cliamp'

# --- File finder ---
alias ff='fzf --preview "bat --style=numbers --color=always {}"'

# --- File listing (eza) ---
# Base flags shared by most aliases: dirs first, icons, git status, colour scale
_eza_base='eza --group-directories-first --icons --git'

alias ls="$_eza_base -lh"                          # long, no hidden
alias l="$_eza_base -lah"                          # long, with hidden
alias lsa="$_eza_base -lah"                        # same as l (muscle memory)
alias ll="$_eza_base -lah --header"                # long + column headers
alias la="$_eza_base -a"                           # short listing, with hidden
alias lD="$_eza_base -lhD"                         # directories only
alias lS="$_eza_base -lah --sort=size"             # sort by size
alias lm="$_eza_base -lah --sort=modified"         # sort by modified time
alias ldot="$_eza_base -lhd .*"                    # dotfiles only
alias lt="$_eza_base --tree --level=2 --long"      # tree (2 levels)
alias lta="$_eza_base --tree --level=2 --long -a"  # tree + hidden
alias tree="$_eza_base --tree"                     # full tree (no depth limit)

# --- Modern CLI replacements ---
alias cat='bat'
alias du='dust'
alias htop='btm'
alias bottom='btm'
alias df='duf'
alias proc='procs'

# --- Extract shortcut (plugin provides `extract`, we alias x) ---
alias x='extract'

# --- Trash-CLI (safer rm) ---
alias rm='trash-put'
alias tp='trash-put'
alias trash='trash-put'
alias tl='trash-list'
alias trashlist='trash-list'
alias tr='trash-restore'
alias trashrestore='trash-restore'
alias te='trash-empty'
alias trashempty='trash-empty'
alias rmdirect='/usr/bin/rm -i'  # escape hatch for real deletion

# --- yay (Arch AUR helper) ---
if (( $+commands[yay] )); then
  alias yaconf='yay -Pg'
  alias yaclean='yay -Sc'
  alias yaclr='yay -Scc'
  alias yaupg='yay -Syu'
  alias yasu='yay -Syu --noconfirm'
  alias yain='yay -S'
  alias yains='yay -U'
  alias yare='yay -R'
  alias yarem='yay -Rns'
  alias yarep='yay -Si'
  alias yareps='yay -Ss'
  alias yaloc='yay -Qi'
  alias yalocs='yay -Qs'
  alias yalst='yay -Qe'
  alias yaorph='yay -Qtd'
  alias yainsd='yay -S --asdeps'
  alias yamir='yay -Syy'
  alias yaupd='yay -Sy'
fi
