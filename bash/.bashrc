#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias cco='claude --continue'
export PS1='\W\$ '

export PATH="$HOME/.local/bin:$PATH"

alias ffgrab='ffmpeg -use_wallclock_as_timestamps 1 \
  -f x11grab -framerate 30 -i :0.0 \
  -f pulse -i default \
  -af "pan=stereo|c0=c0|c1=c0" \
  -vaapi_device /dev/dri/renderD128 \
  -vf "format=nv12,hwupload" \
  -c:v h264_vaapi -qp 23 \
  -c:a aac -b:a 192k'


# opencode
export PATH=/home/luke/.opencode/bin:$PATH
