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

## add font paths
xset fp default
#[[ -d /usr/share/fonts/local ]] && xset +fp /usr/share/fonts/local
[[ -d $HOME/.fonts ]] && xset +fp $HOME/.fonts
xset fp rehash

## turn off screen after 1800s
xset +dpms
xset dpms 1800 1800 1800

## Set background
## feh --bg-fill <filename>
sh $HOME/.fehbg &

## start apps in background
list=(mpd termite)
for app in "${list[@]}"; do
  check=$(ps -C "$app" -opid=)
  if [[ -z "$check" ]]; then
  case "$app" in
    mpd) mpd $HOME/.config/mpd.conf ;;
    termite) termite & ;;
  esac
  fi
done

/usr/bin/dwmvolume
/usr/bin/dwmstatus &
while true; do
  dwm 2> ~/.logs/dwm.log
done
