EDITOR=/usr/bin/emacs
PS1='\w\$ '

PATH="$HOME/.local/bin:$PATH"

HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT="%d/%m/%y %T "

# useful for shotcut
QT_SCALE_FACTOR=1.5

HISTCONTROL=ignoreboth:erasedups
HISTIGNORE="ls:ll:cd:pwd:exit:clear:history"

# for GNU make
NO_COLOR=1
CLICOLOR=0

# for apt
APT_NO_COLOR=1

alias e="${EDITOR}"
