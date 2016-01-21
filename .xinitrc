#!/bin/sh
#
# ~/.xinitrc
#

# turn off screen after 1800s
xset +dpms dpms 1800 1800 1800

# set background
feh --no-fehbg --bg-fill "$HOME/pictures/wallpaper/default" &

# key mapping
setxkbmap -option altwin:alt_super_win -option ctrl:nocaps -option compose:ralt

redshift &
termite &
emacs --daemon &

while true; do
  dwm
done
