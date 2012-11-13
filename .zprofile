## DA K00L WAE
#[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1
if (( UID )); then
  [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx &> ~/.logs/.xlog
fi
