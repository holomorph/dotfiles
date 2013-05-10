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

msg() {
	local mesg=$1; shift
	printf "${GREEN}>> ${ALL_OFF}${BOLD}${mesg}${ALL_OFF}\n" "$*" >&2
}

msg2() {
	local mesg=$1; shift
	printf "${BLUE}   >> ${ALL_OFF}${mesg}\n" "$*" >&2
}

msg "Checking existing "$TERMINFO"..."
if [[ -f "$HOME/.terminfo/s/$TERMINFO" ]]; then
	msg2 "User $TERMINFO already exists!"
	cd "$HOME/.terminfo/"
	msg2 "Removing existing terminfo..."
	rm --force "s/$TERMINFO"
	rmdir "s/"
else
	msg2 "$TERMINFO does not exist"
fi

if [[ ! -d "$HOME/.terminfo" ]]; then
	msg "Creating .terminfo directory..."
	mkdir "$HOME/.terminfo/"
fi

msg "Patching "$TERMINFO"..."
infocmp "$TERMINFO" | sed \
-e 's/^screen[^|]*|[^,]*,/screen-256color|screen-256color with italics support,/' \
-e 's/%?%p1%t;3%/%?%p1%t;3%/' \
-e 's/smso=[^,]*,/smso=\\E[7m,/' \
-e 's/rmso=[^,]*,/rmso=\\E[27m,/' \
-e '$s/$/ sitm=\\E[3m, ritm=\\E[23m,/' > "$HOME/.terminfo/"$TERMINFO".terminfo"
msg "Compiling new terminfo file..."
tic "$HOME/.terminfo/"$TERMINFO".terminfo"
msg "Done!"

unset CDPATH
