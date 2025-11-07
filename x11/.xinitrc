#!/bin/bash

export XCURSOR_THEME=Adwaita
export XSECURELOCK_FONT="-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso10646-1"

xrdb -merge ~/.Xresources

xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &

picom --backend glx --vsync &
exec dbus-launch --exit-with-session emacs -mm

