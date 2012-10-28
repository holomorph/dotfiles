#vt=$(fgconsole 2>/dev/null)
#(( vt == 1 )) && startx -- vt$vt &> ~/.logs/xlog
#unset vt

#startx -- vt$(fgconsole)

## DA K00L WAE
[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1
