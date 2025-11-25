#!/bin/bash

xrdb -merge ~/.Xresources

exec dbus-launch --exit-with-session /usr/bin/cinnamon-session
