# ~/.config/zsh/zstyle.zsh

zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' rehash yes
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' ignore-parents parent pwd

# kill
zstyle ':completion:*:*:*:*:processes' command \
  "ps -u$USER -o pid,comm,stat -w -w"
zstyle ':completion:*:*:kill:*:processes' list-colors \
  '=(#b) #([0-9]#) ([0-9a-z-]#)*=36=0=32'

# apps
zstyle ':completion:*:*:(vim|gvim):*:*files' ignored-patterns \
  '*.(o|aux|out|pdf)'
zstyle ':completion:*:*:zathura:*:*' file-patterns \
  '(#i)*.{ps,pdf,djvu}:files:ps|pdf|djvu\ files *(-/):directories:directories'

# increase max-errors based on length of word
zstyle -e ':completion:*:approximate:*' max-errors \
  'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# users
zstyle ':completion:*:*:*:users' ignored-patterns \
	bin daemon mail ftp http nobody dbus avahi named git bitlbee mpd \
	rtkit ntp usbmux gdm polkitd uuidd sagemath

# ssh/scp/rsync
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
