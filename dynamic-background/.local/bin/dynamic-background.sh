#!/bin/bash

my_swaypid=0
my_swaypid_new=0
killall swaybg

while true; do
  for i in {1..30}; do
    fastfetch --pipe true -l none > /tmp/ff.txt
    echo >> /tmp/ff.txt
    echo "$((30 - i)) minutes to break" >> /tmp/ff.txt
 
    convert ~/Downloads/debian-infinite-large.png   -font "DejaVu-Sans-Mono" -pointsize 32 -fill '#ccccdc'   -annotate +$(( $(identify -format "%w" ~/Downloads/debian-infinite-large.png) / 2 +340 ))+1520 "$(cat /tmp/ff.txt)"   ~/.config/fastfetch_wallpaper.png && rm /tmp/ff.txt
    swaybg -i ~/.config/fastfetch_wallpaper.png -m center &
    my_swaypid_new=$!
    sleep 60
    [ "$my_swaypid" -gt 0 ] && echo "killing old process $my_swaypid" && kill $my_swaypid
    my_swaypid=$my_swaypid_new
    echo "new process is $my_swaypid"
  done
  notify-send -u normal -t 10000 "Break Reminder" "Time to take a screen break!"
done
