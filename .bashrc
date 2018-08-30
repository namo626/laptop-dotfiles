
BASE03=$(tput setaf 234)
BASE02=$(tput setaf 235)
BASE01=$(tput setaf 240)
BASE00=$(tput setaf 241)
BASE0=$(tput setaf 244)
BASE1=$(tput setaf 245)
BASE2=$(tput setaf 254)
BASE3=$(tput setaf 230)
YELLOW=$(tput setaf 136)
ORANGE=$(tput setaf 166)
RED=$(tput setaf 160)
MAGENTA=$(tput setaf 125)
VIOLET=$(tput setaf 61)
BLUE=$(tput setaf 33)
CYAN=$(tput setaf 37)
GREEN=$(tput setaf 64)
RESET=$(tput sgr0)
BOLD=$(tput bold)
BLUE1=$(tput setaf 31)

# export PS1="\h:\w\$ "
export PS1="\[${BOLD}${BLUE1}\]\h:\[$CYAN\]\w\[$BASE0\]${RESET}\$ "

LS_COLORS=$LSCOLORS:'di=1;38;5;38'
export LS_COLORS
alias ls='ls --color=auto'

export PATH="/home/namo/anaconda2/bin:/usr/bin:/usr/sbin:/usr/local/bin:/sbin:/bin:/home/namo/.local/bin"

#complete -cf sudo man which systemctl time ssh rsync
if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
fi
