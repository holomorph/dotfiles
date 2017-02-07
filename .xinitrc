#!/bin/sh
#
# ~/.xinitrc
#

# turn off screen after 1800s
xset +dpms dpms 1800 1800 1800

# set background
feh --no-fehbg --bg-fill "$HOME/pictures/wallpaper/default" &

# key mapping
setxkbmap -option ctrl:nocaps -option compose:ralt

redshift &
termite &
emacs --daemon &

exec dwm
