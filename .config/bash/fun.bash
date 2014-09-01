# bash/fun.bash

n() {
	local dir="$HOME/doc/notes"
	local editor="${EDITOR:-vim}"
	local arg files=()

	for arg; do
		files+=("$dir/$arg")
	done
	if [[ "$@" = @(*.gpg*|*.org*) ]]; then
		editor=(emacsclient -t -avim)
	fi
	command "${editor[@]}" "${files[@]}"
}

_notes() {
	local dir="$HOME/doc/notes"
	local files=($dir/**/"$2"*)

	if [[ -e ${files[0]} ]]; then
		COMPREPLY=("${files[@]##$dir/}")
	fi
}

complete -o default -F _notes n
