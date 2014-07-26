# zsh/util.zsh

cd() {
	builtin cd "$@" && ls;
}

x() {
	if [[ $# -eq 0 ]]; then
		printf '%s\n' "usage: x <archive1> [<archive2> [...]]"
		return 1
	fi

	while (( $# > 0 )); do
		case "$1" in
			*.tar.gz | *.tgz) bsdtar -xvzf "$1";;
			*.tar.bz2 | *.tbz) bsdtar -xvjf "$1";;
			*.7z) 7za x "$1";;
			*.bz2) bunzip2 "$1";;
			*.gz) gunzip -k "$1";;
			*.rar) unrar e -ad "$1";;
			*.zip) unzip "$1";;
			*) bsdtar -xvf "$1";;
		esac

		shift
	done
}

# vim: syn=sh
