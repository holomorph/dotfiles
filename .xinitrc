#!/bin/sh
#
# ~/.xinitrc
#

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# source xinitrc.d scripts
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/* ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

## merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# add font paths
#xset fp default
#[[ -d /usr/share/fonts/local ]] && xset +fp /usr/share/fonts/local
#[[ -d $HOME/.fonts ]] && xset +fp $HOME/.fonts
#xset fp rehash

## turn off screen after 1800s
xset +dpms
xset dpms 1800 1800 1800

## Set background
## feh --bg-fill <filename>
sh $HOME/.fehbg &

## start apps in background
#list=(mpd urxvtd firefox)
list=(mpd termite firefox)
for app in "${list[@]}"; do
  check=$(ps -C "$app" -opid=)
  if [[ -z "$check" ]]; then
	case "$app" in
	  mpd) mpd $HOME/.config/mpd.conf ;;
#	  urxvtd) urxvtd -q -o -f ;;
    termite) termite & ;;
    firefox) firefox & ;;
	esac
  fi
done

#urxvtc &
/usr/bin/dwmstatus &
while true; do
  # Log stderror to a file 
  dwm 2> ~/.logs/dwm.log
  # No error logging
  #dwm >/dev/null 2>&1
done
