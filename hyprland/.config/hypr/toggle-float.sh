#!/bin/bash
# Toggle floating — when floating, center at 70% of screen
win=$(hyprctl activewindow -j)
floating=$(echo "$win" | grep -o '"floating": [a-z]*' | grep -o 'true\|false')

if [ "$floating" = "false" ]; then
    hyprctl --batch "/dispatch togglefloating;/dispatch resizeactive exact 70% 70%;/dispatch centerwindow"
else
    hyprctl dispatch togglefloating
fi
