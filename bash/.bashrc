#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
export PS1='\W\$ '

export PATH="$HOME/.local/bin:$PATH"

alias ffgrab='ffmpeg -f x11grab -framerate 30 -i :0.0   -f pulse -i default.monitor   -f pulse -i default   -filter_complex "[1:a][2:a]amix=inputs=2:duration=first"   -c:v libx264 -preset medium -crf 23   -c:a aac -b:a 192k'
