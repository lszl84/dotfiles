#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $0 input.wav [output.wav]"
    exit 1
fi

INPUT="$1"
OUTPUT="${2:-processed_$1}"

ffmpeg -hide_banner -loglevel quiet -i "$INPUT" -af \
"arnndn=m=$HOME/dotfiles/denoise/sh.rnnn,\
acompressor=threshold=0.05:ratio=2:attack=10:release=100,\
volume=18dB,\
equalizer=f=700:width_type=h:width=100:g=-3,\
equalizer=f=15000:width_type=h:width=2000:g=3" \
"$OUTPUT" 

echo "--- $INPUT:"

ffmpeg -hide_banner -i "$OUTPUT" -filter_complex ebur128 -f null - 2>&1 |grep -A1 "Integrated loudness"
