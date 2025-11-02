#!/bin/bash

export XCURSOR_THEME=Adwaita
export XSECURELOCK_FONT="-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso10646-1"

xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &
xrandr --output HDMI-2 --mode 1920x1080 --rate 100
exec dbus-launch --exit-with-session emacs -mm

