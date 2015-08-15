# zsh/alias.zsh

alias v='vim'
alias g='git'
alias e='emacsclient -t'
alias s='sudoedit'

alias gdb='gdb -q'
alias ncmpcpp='ncmpcpp -c ~/.config/ncmpcpp/config'
alias weechat='weechat -d ~/.config/weechat'

alias grep='grep --color=auto'
alias df='df -h'
alias du='du -h'
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'
alias ln='ln -iv'
alias ls='ls -hF --color=auto --group-directories-first'
alias la='ls -A'
alias ll='ls -l'
alias po='popd'
alias pu='pushd'
alias dirs='dirs -v'

# pastebins
alias ix="curl -F 'f:1=<-' http://ix.io"
alias ptpb="curl -F 'c=<-' https://ptpb.pw"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
