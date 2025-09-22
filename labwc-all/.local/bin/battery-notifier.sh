#!/bin/bash

# Variable to track if the notification has already been shown
notification_sent=0

while true; do
    # Get the current battery percentage
    battery_percentage=$(upower -i $(upower -e | grep 'battery') | grep -i percentage | awk '{gsub("%", "", $2); print $2}')

    # Check if the battery percentage is below 15 and if the notification has not been sent
    if [ "$battery_percentage" -lt 15 ] && [ "$notification_sent" -eq 0 ]; then
        notify-send "Battery low" "Battery at $battery_percentage%"
        notification_sent=1  # Set flag to prevent further notifications until above 15%
    fi

    # Reset notification flag if battery goes above 15%
    if [ "$battery_percentage" -ge 15 ]; then
        notification_sent=0
    fi

    # Wait for 60 seconds before checking again
    sleep 60
done
