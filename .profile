# ~/.profile

export MAILDIR=~/mail

# man page colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[0;30;102m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01m'

# dircolors
source <(dircolors -b "$HOME/.config/dircolors")

# turn on font antialiasing in java
export _JAVA_OPTIONS=-Dawt.useSystemAAFontSettings=lcd

# set location of gtk2 gtkrc (also needed for Qt's gtk style)
export GTK2_RC_FILES=~/.config/gtk-2.0/gtkrc
export MPV_HOME=~/.config/mpv
export ELINKS_CONFDIR=~/.config/elinks
export TERMINFO=~/.local/share/terminfo
export NETHACKOPTIONS=~/.config/nethack/nethackrc
export NOTMUCH_CONFIG=~/.config/notmuch/config

# path
export PATH="$HOME/.local/bin:$PATH"
