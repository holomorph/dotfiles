# zsh/style.zsh

zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' rehash yes
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'

# ssh/scp/rsync
zstyle ':completion:*:(ssh|scp|rsync):*:hosts' ignored-patterns 'localhost*' loopback

# kill
zstyle ':completion:*:*:kill:*:processes' command "ps -u$USER -o pid,%cpu,tty,cmd"

# apps
zstyle ':completion:*:*:(vim|gvim):*:*files' ignored-patterns \
	'*.(o|aux|out|pdf)'
zstyle ':completion:*:*:zathura:*:*' file-patterns \
	'*.(eps|ps|pdf|djvu):files *(-/):directories'

# users
zstyle ':completion:*:*:*:users' ignored-patterns \
	bin daemon mail ftp http uuidd dbus nobody 'systemd*' avahi git bitlbee mpd \
	dnsmasq colord rtkit ntp polkitd sagemath xbmc
