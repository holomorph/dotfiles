#
# ~/.zprofile
#

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

# dircolors
source <(dircolors -b "$HOME/.config/dircolors")

# disable less history file
export LESSHISTFILE=-

# highest compression
export GZIP=-9
export BZIP=-9
export XZ_OPT=-9

# turn on font antialiasing in java
export _JAVA_OPTIONS=-Dawt.useSystemAAFontSettings=lcd

# set location of gtk2 gtkrc (also needed for Qt's gtk style)
export GTK2_RC_FILES=~/.config/gtk-2.0/gtkrc

# path
export PATH="$HOME/.local/bin:$PATH"

if (( UID )); then
  [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx &> "$HOME/.logs/x.log"
fi
