# ~/.config/zsh/bindings.zsh

bindkey -e # emacs

zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey " " magic-space
bindkey "^[[A" up-line-or-beginning-search   # Up
bindkey "^[[B" down-line-or-beginning-search # Down
bindkey "^[[Z" reverse-menu-complete         # Shift+Tab
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^K" kill-line
bindkey "^N" down-line-or-beginning-search
bindkey "^P" up-line-or-beginning-search
bindkey "^U" kill-whole-line
bindkey "^W" vi-backward-kill-word
bindkey "^?" backward-delete-char
bindkey "^[[3~" delete-char
