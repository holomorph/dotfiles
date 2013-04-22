# ~/.bash_profile

# grep colors
export GREP_COLOR='1;32'

# man page colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[03;92m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;37m'

# disable less history file
export LESSHISTFILE=-

# highest compression
export GZIP=-9
export BZIP=-9
export XZ_OPT=-9

# inputrc
export INPUTRC="$HOME/.config/inputrc"

# dircolors
source <(dircolors -b "$HOME/.config/dircolors")

[ -n $BASH -a -r "$HOME/.bashrc" ] && . "$HOME/.bashrc"
