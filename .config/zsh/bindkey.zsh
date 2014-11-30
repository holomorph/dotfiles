# zsh/bindkey.zsh

bindkey -e

autoload -U select-word-style
select-word-style bash

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey " " magic-space
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search
bindkey "^[^I" reverse-menu-complete
bindkey "^[[Z" reverse-menu-complete
bindkey "^D" delete-char
bindkey "^N" down-line-or-beginning-search
bindkey "^P" up-line-or-beginning-search
bindkey "^U" backward-kill-line
bindkey "^W" kill-region
bindkey "^[a" vi-backward-blank-word
bindkey "^[e" vi-forward-blank-word
bindkey "^[^R" history-incremental-pattern-search-backward
bindkey "^[^S" history-incremental-pattern-search-forward
bindkey "^[[3~" delete-char

bindkey -M isearch '^M' accept-search
