#!/bin/bash

# Hardcoded break interval in seconds (1800 seconds = 30 minutes)
BREAK_INTERVAL=1800

while true; do
    # Wait for the specified break interval
    sleep $BREAK_INTERVAL

    # Send a reminder notification
    notify-send "Take a Break!" "Time to stretch and rest for a while."
done
