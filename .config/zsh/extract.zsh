# ~/.config/zsh/extract.zsh

function x() {
	[[ $# -eq 0 ]] && ( echo "usage: x <archive1> [<archive2> [...]]" && exit 1 )

	while (( $# > 0 )); do
		case "$1" in
		(*.7z) 7za x "$1";;
		(*.bz2) bunzip2 "$1";;
		(*.gz) gunzip -k "$1";;
		(*.rar) unrar e -ad "$1";;
		(*) bsdtar -xf "$1";;
		esac

		shift
	done
}

# vim: syn=sh
