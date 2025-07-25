#!/bin/bash

while true; do
  for i in {1..30}; do
    clear
    fastfetch
    sleep 60  # sleep for 1 minute
  done
  notify-send -u normal -t 10000 "Break Reminder" "Time to take a screen break!"
done

