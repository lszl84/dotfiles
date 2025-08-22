#!/bin/bash

killall swaybg

# run first to avoid imagemagick delay
swaybg -i ~/.config/fastfetch_wallpaper.png -m center &
my_swaypid=$!
my_swaypid_new=0

while true; do
  for i in {1..30}; do
    fastfetch --pipe true -l none > /tmp/ff.txt
    echo >> /tmp/ff.txt
    echo "$(date '+%A, %d %B %Y %H:%M'). Break in $((30 - i)) minutes." >> /tmp/ff.txt
    echo >> /tmp/ff.txt
    sudo apt update 2>/dev/null|grep 'can be up' >> /tmp/ff.txt
    updates=$(dnf check-update -q)

	if [ -z "$updates" ]; then
	    echo "No updates available." >>/tmp/ff.txt
	else
	    echo "Updates are available!" >>/tmp/ff.txt
	fi
 
    convert ~/.config/labwc/peakpx.jpg   -font "DejaVu-Sans-Mono" -pointsize 32 -fill '#aaaaca'   -annotate +$(( $(identify -format "%w" ~/.config/labwc/peakpx.jpg) / 2 -150))+1117 "$(cat /tmp/ff.txt)"   ~/.config/fastfetch_wallpaper.png && rm /tmp/ff.txt
    swaybg -i ~/.config/fastfetch_wallpaper.png -m center &
    my_swaypid_new=$!
    sleep 60
    [ "$my_swaypid" -gt 0 ] && echo "killing old process $my_swaypid" && kill $my_swaypid
    my_swaypid=$my_swaypid_new
    echo "new process is $my_swaypid"
  done
  notify-send -u normal -t 10000 "Break Reminder" "Time to take a screen break!"
done
