#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#export XCURSOR_SIZE=32

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
   exec startx
fi
