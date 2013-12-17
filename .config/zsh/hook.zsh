# ~/.config/zsh/hook.zsh

## hooks
case $TERM in
	rxvt* | vte* | xterm* )
		precmd() {
			print -Pn "\e];%m (%~) - Terminal\a"
			vcs_info
		}
		preexec() {
			local cmd=${1[(wr)^(*=*|sudo|-*)]}
			print -Pn "\e];$cmd:q - Terminal\a"
		}
		;;
	*)
		precmd() {
			vcs_info
		}
		;;
esac
