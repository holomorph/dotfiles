#
# ~/.config/zsh/aliases.zsh
#

alias v='vim -p'
alias g='git'

alias df='df -h'
alias du='du -h'
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'
alias ln='ln -iv'
alias ls='ls -hF --color=auto --group-directories-first'
alias la='ls -A'
alias ll="ls++ --potsf"
alias grep="grep --color=auto"

alias ncmpcpp="ncmpcpp -c $HOME/.config/ncmpcpp/config"

# pastebins
alias ix="curl -F 'f:1=<-' http://ix.io"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
