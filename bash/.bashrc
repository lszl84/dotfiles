EDITOR=/usr/bin/emacs
PS1='\w\$ '

MANPATH="$HOME/.local/share/man:"

HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT="%d/%m/%y %T "

HISTCONTROL=ignoreboth:erasedups
HISTIGNORE="ls:ll:cd:pwd:exit:clear:history"

# for GNU make
export NO_COLOR=1

alias e="${EDITOR}"
