
#!/bin/bash

# Find the highest existing number
last_file=$(find . -maxdepth 1 -name "*.wav" | grep -E '[0-9]+\.wav$' | sort -V | tail -n 1)

if [ -z "$last_file" ]; then
    # No files found, start from 001
    next_num=1
else
    # Extract number from last file and increment
    next_num=$(echo "$last_file" | grep -oE '[0-9]+' | tail -n 1)
    next_num=$((10#${next_num#0*} + 1))
fi

# Format as 3-digit number
output_file=$(printf "%03d.wav" $next_num)

echo "Recording to: $output_file"
echo "Press Ctrl+C to stop recording"

# Record audio using default input device
ffmpeg -f pulse -i default -acodec pcm_s16le -ar 44100 -ac 1 "$output_file"
