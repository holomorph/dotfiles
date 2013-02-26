# ~/.config/zsh/zstyle.zsh

zstyle ':completion:*' menu select
#zstyle ':completion:*:matches' group 'yes'
#zstyle ':completion:*:options' description 'yes'
#zstyle ':completion:*:options' auto-description '%d'
#
#zstyle ':completion:*:functions' ignored-patterns '_*'
#zstyle ':completion:*:match:*' original only
#
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:*:*:*:processes' command \
  "ps -u $USER -o pid,user,comm -w -w"

# kill
zstyle ':completion:*:*:kill:*' command 'ps -e -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors \
  '=(#b) #([0-9]#) ([0-9a-z-]#)*=32=0=34'
zstyle ':completion:*:*:kill:*' insert-ids single

# apps
zstyle ':completion:*:*:(vim|gvim):*:*files' ignored-patterns \
	'*~|*.(aux|old|out|pdf)'
zstyle ':completion:*:*:(zathura|mupdf):*:*' file-patterns \
  '(#i)*.{ps,pdf}:files:ps|pdf\ files *(-/):directories:directories'

# increase max-errors based on length of word
zstyle -e ':completion:*:approximate:*' max-errors \
  'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

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

#zstyle ':completion:*' matcher-list \
#	'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#zstyle ':completion:*' list-colors ''
#zstyle ':completion:*' completer \
#	_oldlist _expand _force_rehash _complete _match _approximate
#zstyle ':completion:*' menu select=2

# ssh/scp/rsync
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
