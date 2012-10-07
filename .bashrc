#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# alias ls='ls -hF --color=auto'
# PS1='[\u@\h \W]\$ '

# prompt
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
color_off='\e[0m' # Text Reset
black='\e[0;30m' # Black
red='\e[0;31m' # Red
green='\e[0;32m' # Green
yellow='\e[0;33m' # Yellow
blue='\e[0;34m' # Blue
purple='\e[0;35m' # Purple
cyan='\e[0;36m' # Cyan
white='\e[0;37m' # White
PS1="\[${cyan}\]\u\[${blue}\]@\[${purple}\]\h \[${yellow}\]\w \[${green}\]$ \[${color_off}\]"

export GREP_COLOR="1;31"
alias grep='grep --color=auto'
export LESS="-R"
alias ls='ls -hF --color=auto'
# eval $(dircolors -b $HOME/.dircolors_fix)
