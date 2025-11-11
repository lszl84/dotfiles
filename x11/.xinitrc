#!/bin/bash

xset +dpms
xset s activate
(sleep 1 && xset s reset) &

export XCURSOR_THEME=Adwaita
export XSECURELOCK_FONT="Terminus-14"

xrdb -merge ~/.Xresources
xbindkeys &
xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &

picom --backend glx --vsync &
(sleep 2 && feh --bg-fill ~/Pictures/Wallpapers/pexels-nathanjhilton-19271887.jpg) &

exec dbus-launch --exit-with-session emacs -mm
