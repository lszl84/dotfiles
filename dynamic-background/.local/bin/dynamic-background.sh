#!/bin/sh

my_color="0c3246"
my_bck="$HOME/.config/labwc/ape.jpg"

pidof swaybg && killall swaybg

swaybg -m fill -i "$my_bck" &
my_swaypid1=0
my_swaypid_new1=0
my_swaypid2=0
my_swaypid_new2=0

while true; do
  # 90 iterations of 20s = 30min. TODO: use variables
  for i in $(seq 1 90); do
    fastfetch --pipe true -l none > /tmp/ff.txt
    echo "Updates: $(checkupdates | wc -l)" >> /tmp/ff.txt
    echo >> /tmp/ff.txt
    date '+%A, %d %B %Y %H:%M' >> /tmp/ff.txt
    echo >> /tmp/ff.txt

    magick "$my_bck" -resize 3120x2080^ -gravity center -extent 3120x2080 \
       -gravity none \
       -font "DejaVu-Sans-Mono" -pointsize 28 -fill '#999999' \
       -annotate +80+100 "$(cat /tmp/ff.txt)" \
       /tmp/background_3k.png

    magick "$my_bck" -resize 5120x2880^ -gravity center -extent 5120x2880 \
       -gravity none \
       -font "DejaVu-Sans-Mono" -pointsize 28 -fill '#999999' \
       -annotate +80+100 "$(cat /tmp/ff.txt)" \
       /tmp/background_5k.png
    
    swaybg -o DP-2 -i /tmp/background_5k.png -m fill &
    my_swaypid_new1=$!
    
    swaybg -o eDP-1 -i /tmp/background_3k.png -m fill &
    my_swaypid_new2=$!
    
    # avoid swaybg flicker after kill
    sleep 1
    [ "$my_swaypid1" -gt 0 ] && kill $my_swaypid1
    [ "$my_swaypid2" -gt 0 ] && kill $my_swaypid2

    # total 20 seconds between refreshes
    sleep 19
    my_swaypid1=$my_swaypid_new1
    my_swaypid2=$my_swaypid_new2

    ps aux |grep swaybg
  done
  notify-send -u normal -t 10000 "Break Reminder" "Time to take a screen break!"
done
