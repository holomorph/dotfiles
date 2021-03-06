# ~/.profile

# man page colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[0;30;102m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01m'

# dircolors
source <(dircolors -b <(cat "$HOME/.config/dircolors"/*.conf))

# turn on font antialiasing in java
export _JAVA_OPTIONS=-Dawt.useSystemAAFontSettings=lcd

# set location of gtk2 gtkrc (also needed for Qt's gtk style)
export GTK2_RC_FILES=~/.config/gtk-2.0/gtkrc
export LESSHISTFILE=~/.cache/less/hist
export ELINKS_CONFDIR=~/.config/elinks
export TERMINFO=~/.local/share/terminfo
export NETHACKOPTIONS=~/.config/nethack/nethackrc
export NOTMUCH_CONFIG=~/.config/notmuch/config

# other config
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR"/ssh-agent
export MPD_HOST="$XDG_RUNTIME_DIR"/mpd/socket
export XKB_DEFAULT_OPTIONS=altwin:alt_super_win,ctrl:nocaps,compose:ralt
export GTK_IM_MODULE=xim

# path
export PATH="$HOME/.local/bin:$PATH"

# id
export NAME='Mark Oteiza'
export EMAIL='mvoteiza@udel.edu'
export MAILDIR=~/mail
