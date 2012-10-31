#
# ~/.config/zsh/aliases.zsh
#

alias ssh='eval $(/usr/bin/keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa) && ssh'
#alias emacs="TERM=xterm-256color emacs -nw"
alias ncmpcpp="ncmpcpp -c $HOME/.config/ncmpcpp/config"
alias ls="ls -hF --color=auto --group-directories-first"
alias ll="ls++"
alias grep="grep --color=auto"
#alias gist="jist -p"
#alias range="urxvtc -name ranger -e ranger"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias ix="curl -n -F 'f:1=<-' http://ix.io"
#alias tm="urxvtc -name chatmail -e tmux attach-session -d -t 0"
#alias usbmount="sudo ntfs-3g -o gid=100,fmask=113,dmask=002 /dev/sde1 /mnt/usb"
#alias usbumount="sudo umount /mnt/usb"
#
#
