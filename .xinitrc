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
setxkbmap -option shift:both_shiftlock
xmodmap -e "keycode 135 = Super_R"
xbindkeys

## start apps in background
list=(mpd termite)
for app in "${list[@]}"; do
  check=$(ps -C "$app" -opid=)
  if [[ -z "$check" ]]; then
  case "$app" in
    mpd) mpd "$HOME/.config/mpd.conf" ;;
    termite) termite & ;;
  esac
  fi
done

/usr/bin/dwmvolume
/usr/bin/dwmstatus &
while true; do
  dwm 2> "$HOME/.logs/dwm.log"
done
