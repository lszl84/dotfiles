#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $0 input.wav [output.wav]"
    exit 1
fi

INPUT="$1"
OUTPUT="${2:-processed_$1}"

ffmpeg -hide_banner  -i "$INPUT" -af \
       "volume=21dB,\
alimiter=limit=-1dB:attack=5:release=50,\
highpass=f=100,\
equalizer=f=700:width_type=q:width=0.7:gain=-6,\
highshelf=f=6000:g=6,
volume=8dB" \
"$OUTPUT"

echo "--- $INPUT:"

ffmpeg -hide_banner -i "$OUTPUT" -filter_complex ebur128 -f null - 2>&1 |grep -A1 "Integrated loudness"
