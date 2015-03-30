# ~/.bashrc

[[ $- = *i* ]] || return

shopt -s autocd cdspell dirspell extglob globstar no_empty_cmd_completion
shopt -s cmdhist histappend histverify
stty -ixon

set -o notify

# history
HISTFILE="$HOME/.cache/bash/bash_history"
HISTIGNORE="&:ls:exit:reset:clear"
HISTCONTROL="ignoreboth:erasedups"
HISTSIZE=10000
HISTFILESIZE=20000

# bash completion
if [[ -r "/usr/share/bash-completion/bash_completion" ]]; then
    . "/usr/share/bash-completion/bash_completion"
fi

# configs
for cfg in alias bind fun git util; do
    f="$HOME/.config/bash/$cfg.bash"
    if [[ -r "$f" ]]; then
        . "$f"
    fi
done
unset cfg f

# prompt
PS1="\[\e[1;30m\][\[\e[0;91m\]\u\[\e[1;30m\]@\[\e[0;31m\]\h\$(_vcs_info) \
\[\e[0;93m\]\w\[\e[1;30m\]]\n\[\e[0;34m\] \$\[\e[0m\] "
