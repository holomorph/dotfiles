# ~/.config/bash/fun.sh

n() {
	local arg files=(); for arg; do files+=( ~/"doc/notes/$arg" ); done
	${EDITOR:-vim} "${files[@]}"
}

_notes() {
	local files=($HOME/doc/notes/**/"$2"*)
	[[ -e ${files[0]} ]] && COMPREPLY=( "${files[@]##~/doc/notes/}" )
}

complete -o default -F _notes n
