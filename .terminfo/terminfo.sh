#!/bin/bash
#
# find "vim disp" in tmux FAQ
# http://tmux.svn.sourceforge.net/viewvc/tmux/trunk/FAQ

[[ ! -d "$HOME/.terminfo" ]] && mkdir "$HOME/.terminfo/"
terminfo="screen-256color"
infocmp "$terminfo" | sed \
-e 's/^screen[^|]*|[^,]*,/screen-256color|screen-256color with italics support,/' \
-e 's/%?%p1%t;3%/%?%p1%t;3%/' \
-e 's/smso=[^,]*,/smso=\\E[7m,/' \
-e 's/rmso=[^,]*,/rmso=\\E[27m,/' \
-e '$s/$/ sitm=\\E[3m, ritm=\\E[23m,/' > "$HOME/.terminfo/"$terminfo".terminfo"
tic "$HOME/.terminfo/"$terminfo".terminfo"
