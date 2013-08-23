# ~/.config/zsh/aliases.zsh

alias e='emacs -nw'
alias v='vim'
alias g='git'
alias z='zathura --fork'

alias ncmpcpp='ncmpcpp -c ~/.config/ncmpcpp/config'
alias irssi='irssi --home=~/.config/irssi'
alias weechat='weechat -d ~/.config/weechat'

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

# pastebins
alias ix="curl -F 'f:1=<-' http://ix.io"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

# vim: syn=sh
