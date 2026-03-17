#!/bin/bash
# ~/.xinitrc — Session entry point for dwm (launched by startx)
# Usage: startx

export PATH="$HOME/.local/bin:$PATH"

# HiDPI environment (3120x2080 @ 2x)
# GTK scaling is handled by xsettingsd (Gdk/WindowScalingFactor, Gdk/UnscaledDPI)
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export XCURSOR_SIZE=36

# Load Xresources (Xft.dpi: 192)
[[ -f ~/.config/dwm/xresources ]] && xrdb -merge ~/.config/dwm/xresources

# GTK/xsettings daemon
[[ -f ~/.config/dwm/xsettingsd.conf ]] && xsettingsd -c ~/.config/dwm/xsettingsd.conf &

# Compositor
[[ -f ~/.config/picom/picom.conf ]] && picom --config ~/.config/picom/picom.conf &

# Touchegg (3-finger swipe → Ctrl+Alt+Left/Right → tag switch)
command -v touchegg >/dev/null 2>&1 && touchegg &

# Disable tap-to-click (slick-greeter force-enables it on login)
xinput set-prop "GXTP7863:00 27C6:01E0 Touchpad" "libinput Tapping Enabled" 0 2>/dev/null || true

# Bluetooth mouse: max speed
xinput set-prop "Rival 3 Wireless Gen 2" "libinput Accel Speed" 1.0 2>/dev/null || true

# Touchpad: max speed
xinput set-prop "GXTP7863:00 27C6:01E0 Touchpad" "libinput Accel Speed" 1.0 2>/dev/null || true

# Idle: lock at 20min (xss-lock → dwm-lock), suspend 30s after lock
xset s 1200 0

# Screen locker — dwm built-in locker via xss-lock for IdleHint + sleep inhibitor
# dwm-lock sets _DWM_LOCK property → dwm lock() paints black OverrideRedirect window
command -v xss-lock >/dev/null 2>&1 && xss-lock -l -- ~/.local/bin/dwm-lock &

# Dark theme: xsettingsd handles GTK3, gsettings handles GTK4/libadwaita
command -v gsettings >/dev/null 2>&1 && gsettings set org.gnome.desktop.interface color-scheme prefer-dark 2>/dev/null || true
command -v gsettings >/dev/null 2>&1 && gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark 2>/dev/null || true

# Polkit agent (GTK — handles password prompts for GUI apps)
[[ -x /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 ]] && /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Notification daemon
command -v dunst >/dev/null 2>&1 && dunst &

# Status bar
[[ -x ~/.local/bin/dwm-status ]] && ~/.local/bin/dwm-status &

# Desktop portal (for Firefox/libadwaita dark mode detection)
[[ -x /usr/lib/xdg-desktop-portal-gtk ]] && /usr/lib/xdg-desktop-portal-gtk &
[[ -x /usr/lib/xdg-desktop-portal ]] && /usr/lib/xdg-desktop-portal &

# Start dwm
exec ~/.local/bin/dwm
