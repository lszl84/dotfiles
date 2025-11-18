#!/bin/bash

is_audio_playing() {
    pactl list sinks | grep -q "State: RUNNING"
}

autosuspend() {
    while true; do
        idle_ms=$(xprintidle)
        if [ $idle_ms -gt 1800000 ] && ! is_audio_playing; then
            systemctl suspend
        fi
        sleep 60
    done
}

xset +dpms
xset s activate
(sleep 1 && xset s reset) &

export XCURSOR_THEME=Adwaita
export XSECURELOCK_FONT="Adwaita Mono"

xrdb -merge ~/.Xresources
xbindkeys &
xss-lock -- xsecurelock &

picom --backend glx --vsync &
(sleep 2 && feh --bg-fill $HOME/Pictures/Wallpapers/pexels-nathanjhilton-19271887.jpg) &

autosuspend &

exec dbus-launch --exit-with-session emacs -mm
