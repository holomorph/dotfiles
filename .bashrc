# ~/.bashrc

[[ $- = *i* ]] || return

# bash(1)
shopt -s cdspell dirspell extglob histverify no_empty_cmd_completion
# shopt -s checkwinsize

set -o notify

# history
export HISTFILE="$HOME/.logs/bash_history"
export HISTIGNORE="&:ls:exit:reset:clear"
export HISTCONTROL="ignoreboth:erasedups"
export HISTSIZE=10000
export HISTFILESIZE=20000

# bash completion
[[ -r "/usr/share/bash-completion/bash_completion" ]] && \
	. "/usr/share/bash-completion/bash_completion"

# prompt
PS1="\[\e[1;30m\][\[\e[0;31m\]\u\[\e[1;30m\]@\[\e[0;91m\]\h \
\[\e[0;93m\]\w\[\e[1;30m\]]\n\[\e[0;34m\] \$\[\e[0m\] "

# aliases
alias e='emacs -nw'
alias v='vim'
alias g='git'
alias z='zathura --fork'

alias df='df -h'
alias du='du -h'
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'
alias ln='ln -iv'
alias ls='ls -hF --color=auto --group-directories-first'
alias la='ls -A'
alias ll='ls -l'
alias grep='grep --color=auto'

alias ix="curl -F 'f:1=<-' http://ix.io"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

# bindings
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

bind '"\C-a": beginning-of-line'
bind '"\C-e": end-of-line'
bind '"\C-u": kill-whole-line'

# function
function cd() {
	builtin cd "$@" && ls;
}

# configs
# for cfg in aliases bindings; do
# 	[[ -r "$HOME/.config/bash/"$cfg".sh" ]] && \
# 		. "$HOME/.config/bash/"$cfg".sh"
# done
# unset cfg
