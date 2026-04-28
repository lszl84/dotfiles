#!/usr/bin/env bash
# Replicate this GNOME setup on a fresh Fedora install:
#   - Dash to Panel, ArcMenu, Tiling Shell (installed + configured)
#   - dnf-automatic security-only updates
#
# Run on the target machine. Idempotent — safe to re-run.
# Requires: Fedora 43+ with GNOME, internet, sudo.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

log()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!! \033[0m %s\n' "$*"; }
die()  { printf '\033[1;31mxx \033[0m %s\n' "$*" >&2; exit 1; }

[[ "${XDG_SESSION_TYPE:-}" == "wayland" || "${XDG_SESSION_TYPE:-}" == "x11" ]] \
  || warn "No GNOME session detected — extension activation will need logout/login."

command -v gnome-shell >/dev/null || die "gnome-shell not found. This script targets GNOME."

# ----- 1. Prerequisites -------------------------------------------------------

log "Installing pipx + gnome-extensions-cli"
sudo dnf install -y pipx >/dev/null
pipx install --quiet --force gnome-extensions-cli --system-site-packages >/dev/null
export PATH="$HOME/.local/bin:$PATH"
command -v gext >/dev/null || die "gext not on PATH after pipx install."

# ----- 2. Install extensions --------------------------------------------------

# IDs from extensions.gnome.org URL slugs:
#   1160 = Dash to Panel
#   3628 = ArcMenu
#   7065 = Tiling Shell
EXT_IDS=(1160 3628 7065)
EXT_UUIDS=(
  "dash-to-panel@jderose9.github.com"
  "arcmenu@arcmenu.com"
  "tilingshell@ferrarodomenico.com"
)

for id in "${EXT_IDS[@]}"; do
  log "Installing extension $id"
  gext install "$id" || warn "gext install $id returned non-zero (may already be installed)."
done

# ----- 3. Apply dconf settings ------------------------------------------------

# Detect monitor ID format used by Dash to Panel: "<vendor>-<serial>"
detect_monitor_id() {
  busctl --user --json=short call \
    org.gnome.Mutter.DisplayConfig \
    /org/gnome/Mutter/DisplayConfig \
    org.gnome.Mutter.DisplayConfig \
    GetCurrentState 2>/dev/null \
  | python3 -c '
import sys, json
d = json.load(sys.stdin)
# data[1] = monitors; each monitor[0] = (connector, vendor, product, serial)
mons = d["data"][1]
if not mons:
    sys.exit(1)
_, vendor, _, serial = mons[0][0]
print(f"{vendor}-{serial}")
' 2>/dev/null
}

MONITOR_ID="$(detect_monitor_id || true)"
if [[ -z "${MONITOR_ID}" ]]; then
  warn "Could not detect monitor ID via Mutter DBus."
  warn "Dash to Panel will fall back to defaults — open its prefs to re-anchor."
  MONITOR_ID="UNKNOWN-0x0"
else
  log "Detected monitor ID: $MONITOR_ID"
fi

apply_dconf() {
  local path="$1" file="$2"
  [[ -f "$file" ]] || die "Missing dconf snapshot: $file"
  log "Loading $path"
  dconf load "$path" < "$file"
}

# Dash to Panel — substitute placeholder before loading
TMP_D2P="$(mktemp)"
trap 'rm -f "$TMP_D2P"' EXIT
sed "s/__MONITOR_ID__/${MONITOR_ID}/g" \
  "$DOTFILES_DIR/dash-to-panel.dconf" > "$TMP_D2P"
apply_dconf /org/gnome/shell/extensions/dash-to-panel/ "$TMP_D2P"

apply_dconf /org/gnome/shell/extensions/arcmenu/    "$DOTFILES_DIR/arcmenu.dconf"
apply_dconf /org/gnome/shell/extensions/tilingshell/ "$DOTFILES_DIR/tilingshell.dconf"

# ----- 4. Enable extensions ---------------------------------------------------

for uuid in "${EXT_UUIDS[@]}"; do
  log "Enabling $uuid"
  gnome-extensions enable "$uuid" || warn "Couldn't enable $uuid yet — log out/in and re-run."
done

# ----- 5. dnf-automatic security-only -----------------------------------------

if [[ ! -f /etc/dnf/automatic.conf ]]; then
  log "Installing dnf-automatic config"
  sudo install -m 0644 \
    /usr/share/dnf5/dnf5-plugins/automatic.conf \
    /etc/dnf/automatic.conf
fi

log "Configuring dnf-automatic for security-only auto-apply"
sudo sed -i \
  -e 's/^apply_updates = .*/apply_updates = yes/' \
  -e 's/^upgrade_type = .*/upgrade_type = security/' \
  /etc/dnf/automatic.conf

sudo systemctl enable --now dnf5-automatic.timer

# ----- Done -------------------------------------------------------------------

cat <<EOF

\033[1;32m==> Done.\033[0m

Next step: \033[1mlog out and log back in\033[0m (Wayland needs a fresh shell to
load the extensions). After login your panel, menu and tiling layout should
match the source machine.

Verify:
  gnome-extensions list --enabled
  systemctl list-timers dnf5-automatic.timer

EOF
