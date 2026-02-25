#!/usr/bin/env bash
# Show a red dot (screen shader) when waybar is hidden and battery is discharging.

BAT=/sys/class/power_supply/BAT0
SHADER=~/.config/hypr/low-battery-tint.glsl
THRESHOLD=15
ACTIVE=false

while true; do
    capacity=$(<"$BAT/capacity")
    status=$(<"$BAT/status")
    # waybar moves to "top" layer when visible, "bottom" when hidden
    waybar_on_top=$(hyprctl layers 2>/dev/null | grep -B1 "waybar" | grep -c "top")

    if (( capacity <= THRESHOLD )) && [[ "$status" == "Discharging" ]] && (( waybar_on_top == 0 )); then
        if ! $ACTIVE; then
            hyprctl keyword decoration:screen_shader "$SHADER"
            ACTIVE=true
        fi
    else
        if $ACTIVE; then
            hyprctl keyword decoration:screen_shader ""
            ACTIVE=false
        fi
    fi

    sleep 5
done
