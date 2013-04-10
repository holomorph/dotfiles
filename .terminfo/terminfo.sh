#!/bin/bash
#
# find "vim disp" in tmux FAQ
# http://tmux.svn.sourceforge.net/viewvc/tmux/trunk/FAQ

TERMINFO="screen-256color"

ALL_OFF="\e[1;0m"
BOLD="\e[1;1m"
BLUE="${BOLD}\e[1;34m"
GREEN="${BOLD}\e[1;32m"
RED="${BOLD}\e[1;31m"
YELLOW="${BOLD}\e[1;33m"

echo -e "${BLUE}>>${ALL_OFF}${BOLD} Checking existing $TERMINFO...${ALL_OFF}"
if [[ -f "$HOME/.terminfo/s/$TERMINFO" ]]; then
	echo -e "${YELLOW}   >>${ALL_OFF} User $TERMINFO already exists!"
	cd "$HOME/.terminfo/"
	echo -e "${YELLOW}   >>${ALL_OFF} Removing existing terminfo..."
	rm -f "s/$TERMINFO"
	rmdir "s/"
else
	echo -e "${YELLOW}   >>${ALL_OFF} $TERMINFO does not exist"
fi

if [[ ! -d "$HOME/.terminfo" ]]; then
	echo -e "${BLUE}>>${ALL_OFF}${BOLD} Creating .terminfo directory... ${ALL_OFF}"
	mkdir "$HOME/.terminfo/"
fi

echo -e "${BLUE}>>${ALL_OFF}${BOLD} Patching $TERMINFO... ${ALL_OFF}"
infocmp "$TERMINFO" | sed \
-e 's/^screen[^|]*|[^,]*,/screen-256color|screen-256color with italics support,/' \
-e 's/%?%p1%t;3%/%?%p1%t;3%/' \
-e 's/smso=[^,]*,/smso=\\E[7m,/' \
-e 's/rmso=[^,]*,/rmso=\\E[27m,/' \
-e '$s/$/ sitm=\\E[3m, ritm=\\E[23m,/' > "$HOME/.terminfo/"$TERMINFO".terminfo"
echo -e "${BLUE}>>${ALL_OFF}${BOLD} Compiling new terminfo file... ${ALL_OFF}"
tic "$HOME/.terminfo/"$TERMINFO".terminfo"
echo -e "${GREEN}>>${ALL_OFF}${BOLD} Done! ${ALL_OFF}"

unset ALL_OFF BOLD BLUE GREEN RED YELLOW
unset TERMINFO
unset CDPATH
