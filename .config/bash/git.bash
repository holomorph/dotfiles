# bash/git.bash
#
# Git - parts borrowed from grawity
# Parts borrowed from git/contrib/completion/git-prompt.sh
# other parts from zsh/functions/VCS_Info/Backends/VCS_INFO_get_data_git

_git_info() {
	local git="$1"
	local br= sg= un= st= ac=

	if [[ -f $git/rebase-merge/interactive ]]; then
		br=$(<"$git/rebase-merge/head-name")
		ac='rebase-i'
	elif [[ -d $git/rebase-merge ]]; then
		br=$(<"$git/rebase-merge/head-name")
		ac='rebase-m'
	else
		br=$(git symbolic-ref HEAD 2>/dev/null ||
			git describe --tags --exact-match HEAD 2>/dev/null ||
			git rev-parse --short HEAD 2>/dev/null ||
			echo 'unknown')
		br=${br#refs/heads/}

		if [[ -f $git/rebase-apply/rebasing ]]; then
			ac='rebase'
		elif [[ -f $git/rebase-apply/applying ]]; then
			ac='am'
		elif [[ -d $git/rebase-apply ]]; then
			ac='am/rebase'
		elif [[ -f $git/MERGE_HEAD ]]; then
			ac='merge'
		elif [[ -f $git/CHERRY_PICK_HEAD ]]; then
			ac='cherry'
		elif [[ -f $git/BISECT_LOG ]]; then
			ac='bisect'
		fi
	fi

	## staged is more expensive
	local git_size=($(du -sk $git))
	if (( ${git_size[0]} < 50000)); then
		git diff-index --cached --quiet --ignore-submodules HEAD 2>/dev/null
		(( $? && $? != 128 )) && sg='+'
	fi

	git diff --no-ext-diff --ignore-submodules --quiet --exit-code 2>/dev/null || un='*'
	[[ -s $git/refs/stash ]] && st='#'
	[[ -n $ac ]] && ac=${ac:+" $ac"}

	printf " \e[95m%s\e[1;92m%s\e[93m%s\e[96m%s\e[0;96m%s" \
		"$br" "$sg" "$un" "$st" "$ac"
}

_vcs_info() {
	local git=

	if ! command -v git >&/dev/null; then
		return
	elif [[ $GIT_DIR && -d $GIT_DIR ]]; then
		git=$GIT_DIR
	elif [[ ! $GIT_DIR && -d .git ]]; then
		git=.git
	else
		git=$(git rev-parse --git-dir 2>/dev/null)
	fi

	[[ $git ]] && _git_info $git
}
