EDITOR=/usr/bin/mg
PS1='\w\$ '

CC=clang
CXX=clang++
CMAKE_GENERATOR=Ninja

MANPATH="$HOME/.local/share/man:"

HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT="%d/%m/%y %T "

HISTCONTROL=ignoreboth:erasedups
HISTIGNORE="ls:ll:cd:pwd:exit:clear:history"
