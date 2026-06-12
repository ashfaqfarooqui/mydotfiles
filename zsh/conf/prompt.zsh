# ============================================================================
# Prompt & lazy-loaded tools
# ============================================================================

# --- Zoxide (smart cd) — lazy-loaded on first use ---
z() {
  unfunction z
  eval "$(zoxide init zsh)"
  z "$@"
}

# --- Tmuxifier — lazy-loaded on first use ---
tmuxifier() {
  unfunction tmuxifier
  eval "$(tmuxifier init -)"
  tmuxifier "$@"
}

# --- Starship prompt (fast enough to load eagerly) ---
eval "$(starship init zsh)"
