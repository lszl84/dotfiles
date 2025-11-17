#!/bin/bash

xset +dpms
xset s activate
(sleep 1 && xset s reset) &

export XCURSOR_THEME=Adwaita
export XSECURELOCK_FONT="Adwaita Mono"

xrdb -merge ~/.Xresources
xbindkeys &
xss-lock -- xsecurelock &

picom --backend glx --vsync &
(sleep 2 && feh --bg-fill $HOME/Pictures/Wallpapers/pexels-castorlystock-3693214.jpg) &

exec dbus-launch --exit-with-session emacs -mm
