#!/bin/sh

my_color="0c3246"

pidof swaybg && killall swaybg

# run first to avoid imagemagick delay
swaybg -c "$my_color" &
my_swaypid=$!
my_swaypid_new=0

while true; do
  # 90 iterations of 20s = 30min. TODO: use variables
  doas apk update 2>&1 >/dev/null &
  for i in $(seq 1 90); do
    fastfetch --pipe true -l none > /tmp/ff.txt
    echo >> /tmp/ff.txt
    date '+%A, %d %B %Y %H:%M' >> /tmp/ff.txt
    echo >> /tmp/ff.txt
    [ "$(apk version -l '<' | wc -l)" -gt 1 ] && echo "Updates are available!" >> /tmp/ff.txt
 
    magick -background transparent -font "DejaVu-Sans-Mono" -pointsize 32 -fill '#eeeeee' label:"$(cat /tmp/ff.txt)" /tmp/background.png
    swaybg -c "$my_color" -m center -i /tmp/background.png &
    my_swaypid_new=$!

    # avoid swaybg flicker after kill
    sleep 1 
    [ "$my_swaypid" -gt 0 ] && kill $my_swaypid

    # total 20 seconds between refreshes
    sleep 19
    my_swaypid=$my_swaypid_new
  done
  notify-send -u normal -t 10000 "Break Reminder" "Time to take a screen break!"
done
