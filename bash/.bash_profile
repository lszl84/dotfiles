# Auto-start labwc with DBus on TTY1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec dbus-launch --exit-with-session labwc
fi
