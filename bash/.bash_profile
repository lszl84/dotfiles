#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -z "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    wm=$(cat ~/.config/current-wm 2>/dev/null || echo "dwm")
    case "$wm" in
        hyprland) exec start-hyprland-wrapper ;;
        *)        exec startx ;;
    esac
fi
