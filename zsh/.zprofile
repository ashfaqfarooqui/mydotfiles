# ============================================================================
# .zprofile — login shell only
# Runs once per login session (TTY login, SSH without a display manager).
# Keep this minimal — environment variables belong in .zshenv.
# ============================================================================

# Ensure .profile is sourced for zsh login shells.
# Display managers (GDM, SDDM, etc.) already source .profile for the session,
# but TTY logins and SSH sessions do not — this covers those cases.
[[ -f "$HOME/.profile" ]] && emulate sh -c 'source "$HOME/.profile"'
