# ~/.config/zsh/zstyle.zsh

zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' rehash yes
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' completer _complete _match
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' matcher-list 'r:|.=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'

# kill
zstyle ':completion:*:*:kill:*:processes' command \
  "ps -u$USER -o pid,%cpu,tty,cputime,cmd"

# apps
zstyle ':completion:*:*:(vim|gvim):*:*files' ignored-patterns \
	'*.(o|aux|out|pdf)'
zstyle ':completion:*:*:zathura:*:*' file-patterns \
	'*.(eps|ps|pdf|djvu):files *(-/):directories'

# users
zstyle ':completion:*:*:*:users' ignored-patterns \
	bin daemon mail ftp http nobody dbus avahi named git bitlbee mpd \
	rtkit ntp usbmux gdm polkitd uuidd sagemath xbmc

# ssh/scp/rsync
zstyle ':completion:*:(scp|rsync):*' group-order \
	files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' group-order \
	hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts' ignored-patterns \
	'*.*' loopback localhost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns \
	'*.*' loopback localhost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns \
	'<->.<->.<->.<->' '^*.*' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns \
	'^<->.<->.<->.<->' '127.0.0.<->'
