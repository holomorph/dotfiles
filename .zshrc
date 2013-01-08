#
# ~/.zshrc
#

autoload -U compinit && compinit
autoload -U colors && colors
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
autoload -U vcs_info

setopt correct
setopt extended_glob
setopt extended_history share_history
setopt hist_find_no_dups hist_ignore_dups hist_verify
setopt prompt_subst
unsetopt flow_control

HISTSIZE=10000
SAVEHIST=10000
HISTFILE="$HOME/.logs/.zhistory"

## Enhanced Tab Completion
#
zstyle ':completion:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:match:*' original only

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
zstyle ':completion:*:*:zathura:*:*' file-patterns \
  '(#i)*.{ps,pdf}:files:ps|pdf\ files *(-/):directories:directories'

# increase max-errors based on length of word
zstyle -e ':completion:*:approximate:*' max-errors \
  'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Zstyle
#
[[ -e "$HOME/.config/zsh/zstyle.zsh" ]] && \
	eval $(dircolors -b "$HOME/.config/zsh/zstyle.zsh")

# Prompt Settings
#
if [[ -s "$HOME/.config/zsh/prompt.zsh" ]]; then
  source "$HOME/.config/zsh/prompt.zsh"
  PROMPT='%(?..%B%F{red}exit %?%f%b
)'\
'$(vcs_info && echo $vcs_info_msg_0_)%b'\
'%F{black}[%F{11}%~%F{black}]%f'\
'%(!.%F{red}#%f.%F{blue}$%f) '
else
  PROMPT='%(?..%B%F{red}exit %?%f%b
)'\
'%F{black}[%F{11}%~%F{black}]%f'\
'%(!.%F{red}#%f.%F{blue}$%f) '
fi
SPROMPT="Correct $fg_bold[red]%R$reset_color to $fg_bold[green]%r$reset_color [nyae]? "

# Tmux
#
#if which tmux 2>&1 >/dev/null; then
#  # if not inside a tmux session, and if no session is started,
#  # then start a new session
#  test -z "$TMUX" && (tmux attach || tmux new-session)
#fi
#if which tmux 2>&1 >/dev/null; then
#  # if no session is started, start a new session
#  test -z ${TMUX} && tmux
#  # when quitting tmux, try to attach
#  while test -z ${TMUX}; do
#    tmux attach || break
#  done
#fi

# Dynamic Window Title
#
case $TERM in
  (x|a|ml|dt|E)term*|(u|)rxvt*)
    precmd () { print -Pn "\e]0;%n@%M:%~\a" }
    preexec () { print -Pn "\e]0;%n@%M:%~ ($1)\a" }
    ;;
  screen*)
    precmd () {
      print -Pn "\e]83;title - \"$1\"\a"
      print -Pn "\e]0;%n@%M:%~\a"
    }
    preexec () {
      print -Pn "\e]83;title - \"$1\"\a"
      print -Pn "\e]0;%n@%M:%~ ($1)\a"
    }
    ;;
esac

# Keybindings
#
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey "\e[1~" beginning-of-line # Home (tmux)
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[4~" end-of-line # End (tmux)
bindkey "\e[5~" beginning-of-history # PgUp
bindkey "\e[6~" end-of-history # PgDn
bindkey "\e[7~" beginning-of-line # Home (rxvt)
bindkey "\e[8~" end-of-line # End (rxvt)
bindkey "\e[Z" reverse-menu-complete # Shift+Tab
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey ' ' magic-space
bindkey "^?" backward-delete-char
bindkey -M viins "^N" down-line-or-beginning-search
bindkey -M viins "^P" up-line-or-beginning-search
bindkey -M viins "jj" vi-cmd-mode
bindkey -M vicmd "^R" redo
bindkey -M vicmd "u" undo
bindkey -M vicmd "/" history-incremental-search-forward
bindkey -M vicmd "?" history-incremental-search-backward

# Dircolors
#
[[ -e "$HOME/.config/dircolors" ]] && \
  eval $(dircolors -b "$HOME/.config/dircolors")

# Aliases
#
[[ -e "$HOME/.config/zsh/aliases.zsh" ]] && \
  source "$HOME/.config/zsh/aliases.zsh"

# Extract function
#
[[ -e "$HOME/.config/zsh/extract.zsh" ]] && \
  source "$HOME/.config/zsh/extract.zsh"
