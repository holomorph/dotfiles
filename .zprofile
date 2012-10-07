#vt=$(fgconsole 2>/dev/null)
#(( vt == 1 )) && startx -- vt$vt &> ~/.logs/xlog
#unset vt
startx -- vt$(fgconsole)
