#!/bin/bash

while true; do
  for i in {1..30}; do
    clear
    fastfetch
     echo "$((30 - i)) minutes to break"
    sleep 60  # sleep for 1 minute
  done
  notify-send -u normal -t 10000 "Break Reminder" "Time to take a screen break!"
done

