# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.

# ============================================================================
# PATH and Environment Setup (consolidated for faster startup)
# ============================================================================
export PATH="$HOME/.tmuxifier/bin:$HOME/.bun/bin:$HOME/opt/rocm/bin:$HOME/Downloads/nusmv/NuSMV-2.7.1-linux64/bin:$PATH"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}/opt/rocm/lib:/opt/rocm/lib64"
export BUN_INSTALL="$HOME/.bun"

# Source environment files (only if not already sourced in .profile)
[[ -z "$CARGO_ENV_LOADED" ]] && . "$HOME/.cargo/env" && export CARGO_ENV_LOADED=1
[[ -z "$LOCAL_BIN_LOADED" ]] && . "$HOME/.local/bin/env" && export LOCAL_BIN_LOADED=1
[[ -z "$ATUIN_ENV_LOADED" ]] && . "$HOME/.atuin/bin/env" && export ATUIN_ENV_LOADED=1

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="powerlevel10k/powerlevel10k"


# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
 ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
#for ssh agent

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Optimized plugin list - only essentials for maximum performance
# Removed: command-not-found (slow), cp, colorize, sudo (minimal benefit)
# Removed: fzf plugin (redundant with manual setup)
# Removed: uv plugin (lazy loaded instead for performance)
plugins=(
    sudo
git
extract
archlinux
jsontools
python
poetry
fzf-tab
zsh-autosuggestions
zsh-syntax-highlighting  # Keep at end - must be last or near-last
)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
export ALTERNATE_EDITOR="nvim"

export EDITOR='nvim'



# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
#export SSH_KEY_PATH="~/.ssh/id_rsa"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias nvim-kickstart='NVIM_APPNAME="nvim-kickstart" nvim'
alias nvim-personal='NVIM_APPNAME="nvim-personal" nvim'
alias zshconfig="nvim ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias e="emacsclient -c -a emacs"   
alias et="emacsclient -t"
alias v="nvim"
alias s="kitten ssh"
alias vim="nvim"
alias vi="nvim"
alias ff='fzf --preview "bat --style=numbers --color=always {}"'
alias cd="z"
alias ls='eza -lh --group-directories-first --icons'
alias lsa='eza -lha --group-directories-first --icons'
alias lt='eza --tree --level=2 --long --icons --git'
alias lta='lt -a'
alias cat='bat'
alias du="dust"
alias htop="btm"
alias bottom="btm"
alias df="duf"
alias proc="procs"

# Trash-CLI aliases (safer file deletion)
alias rm='trash-put'
alias tp='trash-put'
alias trash='trash-put'
alias tl='trash-list'
alias trashlist='trash-list'
alias tr='trash-restore'
alias trashrestore='trash-restore'
alias te='trash-empty'
alias trashempty='trash-empty'
# Use actual rm when you really need it
alias rmdirect='/usr/bin/rm -i'

ef() { fzf | xargs -r -I % $EDITOR % ;}
ec() { du -a ~/mydotfiles/* | awk '{print $2}' | fzf | xargs -r $EDITOR ;}


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
#[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#b4befe,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8 \
--color=selected-bg:#45475a \
--multi"

show_file_or_dir_preview="if [ -d {} ]; then eza --tree --color=always {} | head -200; else bat -n --color=always --line-range :500 {}; fi"

export FZF_CTRL_T_OPTS="--preview '$show_file_or_dir_preview'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

# Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'eza --tree --color=always {} | head -200' "$@" ;;
    export|unset) fzf --preview "eval 'echo \${}'"         "$@" ;;
    ssh)          fzf --preview 'dig {}'                   "$@" ;;
    *)            fzf --preview "$show_file_or_dir_preview" "$@" ;;
  esac
}

export GPG_TTY=$(tty)
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# ============================================================================
# Lazy Loading Setup for Maximum Performance
# ============================================================================
# Tools are initialized only when first used, dramatically speeding up shell startup

# Lazy load zoxide
z() {
  unfunction z
  eval "$(zoxide init zsh)"
  z "$@"
}

# FZF - Load immediately (fast and frequently used)
eval "$(fzf --zsh)"

# Lazy load tmuxifier
tmuxifier() {
  unfunction tmuxifier
  eval "$(tmuxifier init -)"
  tmuxifier "$@"
}

# UV completions (loaded immediately - fast enough)
eval "$(uv generate-shell-completion zsh)"
eval "$(uvx --generate-shell-completion zsh)"

# Initialize starship (fast enough to load immediately)
eval "$(starship init zsh)"

# Initialize atuin (history search - load immediately for Ctrl-R)
eval "$(atuin init zsh)"

zstyle ':completion:*' menu select
fpath+=~/.zfunc

# SSH configuration
export SSH_AUTH_SOCK=/home/ashfaqf/.bitwarden-ssh-agent.sock


# Quickly switch to a worktree directory
swtree() {
  local dir
  dir=$(git worktree list | grep "$1" | awk '{print $1}')
  if [[ -d "$dir" ]]; then
    cd "$dir" || echo "❌ Worktree not found"
  else
    echo "❌ Worktree '$1' not found"
  fi
}

# Create a new worktree
newtree() {
  if [[ $# -ne 2 ]]; then
    echo "Usage: newtree <directory> <branch>"
    return 1
  fi
  git worktree add "$1" "$2"
}

# Remove a worktree
rmtree() {
  if [[ $# -ne 1 ]]; then
    echo "Usage: rmtree <directory>"
    return 1
  fi
  git worktree remove "$1"
}

# Show status of all worktrees
worktree-status() {
  for dir in $(git worktree list | awk '{print $1}' | tail -n +2); do
    echo "📂 Checking status in: $dir"
    (cd "$dir" && git status --short)
  done
}
#compdef opencode
###-begin-opencode-completions-###
#
# yargs command completion script
#
# Installation: opencode completion >> ~/.zshrc
#    or opencode completion >> ~/.zprofile on OSX.
#
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
if [[ "'${zsh_eval_context[-1]}" == "loadautofunc" ]]; then
  _opencode_yargs_completions "$@"
else
  compdef _opencode_yargs_completions opencode
fi
###-end-opencode-completions-###

