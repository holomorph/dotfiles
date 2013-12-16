#!/bin/sh
#
# ~/.xinitrc
#

## turn off screen after 1800s
xset +dpms
xset dpms 1800 1800 1800

## Set background
feh --no-fehbg --bg-fill "$HOME/pictures/wallpaper/default" &

## key mapping
setxkbmap -option ctrl:nocaps
xmodmap -e "keycode 135 = Super_R"

[[ -z $(pgrep termite) ]] && termite &
[[ -z $(pgrep emacs) ]] && emacs --daemon &

while true; do
  dwm 2> "$HOME/.logs/dwm.log"
done
