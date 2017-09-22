# zsh/style.zsh

zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.cache/zsh/zcompcache"
zstyle ':completion:*' rehash yes
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' matcher-list '+r:|.=* r:|=*'

# ssh/scp/rsync
zstyle ':completion:*:(ssh|scp|rsync):*:hosts' ignored-patterns 'localhost*' loopback
zstyle -e ':completion:*:hosts' hosts 'reply=()'

# kill
zstyle ':completion:*:*:kill:*:processes' command "ps -u$USER -o pid,%cpu,tty,cmd"

# users
zstyle ':completion:*:*:*:users' ignored-patterns \
	bin daemon mail ftp http uuidd dbus nobody 'systemd*' avahi git bitlbee mpd \
	dnsmasq colord rtkit ntp polkitd sagemath xbmc
