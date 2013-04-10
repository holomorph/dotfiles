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

printf "${BLUE}>>${ALL_OFF}${BOLD} Checking existing $TERMINFO...${ALL_OFF}\n"
if [[ -f "$HOME/.terminfo/s/$TERMINFO" ]]; then
	printf "${YELLOW}   >>${ALL_OFF} User $TERMINFO already exists!\n"
	cd "$HOME/.terminfo/"
	printf "${YELLOW}   >>${ALL_OFF} Removing existing terminfo...\n"
	rm -f "s/$TERMINFO"
	rmdir "s/"
else
	printf "${YELLOW}   >>${ALL_OFF} $TERMINFO does not exist\n"
fi

if [[ ! -d "$HOME/.terminfo" ]]; then
	printf "${BLUE}>>${ALL_OFF}${BOLD} Creating .terminfo directory... ${ALL_OFF}\n"
	mkdir "$HOME/.terminfo/"
fi

printf "${BLUE}>>${ALL_OFF}${BOLD} Patching $TERMINFO... ${ALL_OFF}\n"
infocmp "$TERMINFO" | sed \
-e 's/^screen[^|]*|[^,]*,/screen-256color|screen-256color with italics support,/' \
-e 's/%?%p1%t;3%/%?%p1%t;3%/' \
-e 's/smso=[^,]*,/smso=\\E[7m,/' \
-e 's/rmso=[^,]*,/rmso=\\E[27m,/' \
-e '$s/$/ sitm=\\E[3m, ritm=\\E[23m,/' > "$HOME/.terminfo/"$TERMINFO".terminfo"
printf "${BLUE}>>${ALL_OFF}${BOLD} Compiling new terminfo file... ${ALL_OFF}\n"
tic "$HOME/.terminfo/"$TERMINFO".terminfo"
printf "${GREEN}>>${ALL_OFF}${BOLD} Done! ${ALL_OFF}\n"
