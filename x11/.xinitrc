#!/bin/bash

xrdb -merge ~/.Xresources

export XCURSOR_SIZE=32

emacs --daemon

exec dbus-launch --exit-with-session startxfce4

