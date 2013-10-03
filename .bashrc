# ~/.bashrc

[[ $- = *i* ]] || return

## bash(1)
shopt -s cdspell dirspell extglob globstar histverify no_empty_cmd_completion
# shopt -s checkwinsize

set -o notify

## history
export HISTFILE="$HOME/.logs/bash_history"
export HISTIGNORE="&:ls:exit:reset:clear"
export HISTCONTROL="ignoreboth:erasedups"
export HISTSIZE=10000
export HISTFILESIZE=20000

## bash completion
[[ -r "/usr/share/bash-completion/bash_completion" ]] && \
	. "/usr/share/bash-completion/bash_completion"

## prompt
PS1="\[\e[1;30m\][\[\e[0;31m\]\u\[\e[1;30m\]@\[\e[0;91m\]\h \
\[\e[0;93m\]\w\[\e[1;30m\]]\n\[\e[0;34m\] \$\[\e[0m\] "

## configs
for cfg in aliases bindings fun util; do
	[[ -r "$HOME/.config/bash/$cfg.sh" ]] && \
		. "$HOME/.config/bash/$cfg.sh"
done
unset cfg
