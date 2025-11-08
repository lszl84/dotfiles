#!/bin/bash

xset +dpms
xset s activate
(sleep 2 && xset s reset) &

export XCURSOR_THEME=Adwaita
export XSECURELOCK_FONT="Terminus-14"

xrdb -merge ~/.Xresources
xbindkeys &
xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &

picom --backend glx --vsync &
exec dbus-launch --exit-with-session emacs -mm

