#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $0 full_stream_url"
    exit 1
fi

ffmpeg -vaapi_device /dev/dri/renderD128 -f x11grab -framerate 30 -i :0.0 -f lavfi -i anullsrc=channel_layout=stereo:sample_rate=44100 -vf 'format=nv12,hwupload' -c:v h264_vaapi -profile:v high -level 4.2 -g 60 -keyint_min 60 -b:v 4500k -maxrate 4500k -bufsize 9000k -c:a aac -b:a 128k -f flv "$1"
