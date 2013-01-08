#
# ~/.config/zsh/zstyle.zsh
#

#zstyle ':completion:*:corrections' format '%B%F{green}>> %d (errors: %e)%f%b'
#zstyle ':completion:*:descriptions' format '%B%F{magenta}>> %d%f%b'
#zstyle ':completion:*:messages' format '%B%F{cyan}>> %d%f%b'
#zstyle ':completion:*:warnings' format '%B%F{red}>> no matches found%f%b'
#zstyle ':completion:*:default' list-prompt '%B%S%M matches%s%b'
#zstyle ':completion:*' format '%B%F{cyan}>> %d%f%b'
#zstyle ':completion:*' group-name ''
#zstyle ':completion:*' verbose yes

zstyle ':completion:*:*:*:users' ignored-patterns \
	bin daemon mail ftp http nobody dbus avahi named git bitlbee mpd \
	rtkit ntp usbmux gdm

zstyle ':completion:*' matcher-list \
	'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' completer \
	_oldlist _expand _force_rehash _complete _match _approximate
zstyle ':completion:*' menu select=2

# ssh/scp/rsync {{{2
#
zstyle ':completion:*:(scp|rsync):*' tag-order \
	'hosts:-host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order \
	files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
	'hosts:-host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order \
	hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns \
	'*.*' loopback localhost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns \
	'<->.<->.<->.<->' '^*.*' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns \
	'^<->.<->.<->.<->' '127.0.0.<->'
