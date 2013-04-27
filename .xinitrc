#!/bin/sh
#
# ~/.xinitrc
#

## source xinitrc.d scripts
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/* ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

## turn off screen after 1800s
xset +dpms
xset dpms 1800 1800 1800

## Set background
## feh --bg-fill <filename>
sh "$HOME/.fehbg" &

## key mapping
setxkbmap -option ctrl:nocaps
xmodmap -e "keycode 135 = Super_R"
xbindkeys

systemd --user &

[[ -z $(pidof -s termite) ]] && termite &

/usr/bin/dwmstatus &
while true; do
  dwm 2> "$HOME/.logs/dwm.log"
done
