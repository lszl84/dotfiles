#!/usr/bin/env bash
# Symlink userChrome.css + user.js into the Firefox default-release profile.
# Profile dirname has a random prefix that varies per install, so plain stow
# can't target it — this script finds the profile, then links the canonical
# copies in this directory into it.
#
# Idempotent: re-running replaces existing symlinks, refuses to overwrite
# real files (says so and exits non-zero).

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

log()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!! \033[0m %s\n' "$*"; }
die()  { printf '\033[1;31mxx \033[0m %s\n' "$*" >&2; exit 1; }

# Firefox profile root — Fedora/distro packages put it under XDG_CONFIG_HOME,
# upstream tarball/Snap/Flatpak builds use ~/.mozilla. Try both.
CANDIDATES=(
  "${XDG_CONFIG_HOME:-$HOME/.config}/mozilla/firefox"
  "$HOME/.mozilla/firefox"
)

PROFILE_ROOT=""
for d in "${CANDIDATES[@]}"; do
  if [[ -d "$d" ]]; then
    PROFILE_ROOT="$d"
    break
  fi
done

[[ -n "$PROFILE_ROOT" ]] || die "No Firefox profile root found. Run Firefox once first to create it."

# Find the default-release profile dir (the random prefix differs per install).
shopt -s nullglob
PROFILES=( "$PROFILE_ROOT"/*.default-release )
shopt -u nullglob

(( ${#PROFILES[@]} >= 1 )) \
  || die "No *.default-release profile under $PROFILE_ROOT. Launch Firefox once and retry."
(( ${#PROFILES[@]} == 1 )) \
  || die "Multiple default-release profiles found, refusing to guess: ${PROFILES[*]}"

PROFILE="${PROFILES[0]}"
log "Target profile: $PROFILE"

# Link a single source path into a destination path inside the profile.
# Replaces existing symlinks; refuses to overwrite a real file.
link_into_profile() {
  local src="$1" dst="$2"
  [[ -e "$src" ]] || die "Missing source: $src"
  mkdir -p "$(dirname "$dst")"
  if [[ -L "$dst" ]]; then
    rm -f "$dst"
  elif [[ -e "$dst" ]]; then
    die "Refusing to overwrite real file: $dst (move it aside, then re-run)"
  fi
  ln -s "$src" "$dst"
  log "  $dst -> $src"
}

link_into_profile "$DOTFILES_DIR/chrome/userChrome.css" "$PROFILE/chrome/userChrome.css"
link_into_profile "$DOTFILES_DIR/user.js"               "$PROFILE/user.js"

cat <<EOF

\033[1;32m==> Done.\033[0m

Fully quit Firefox (close all windows) and reopen for changes to take effect.
EOF
