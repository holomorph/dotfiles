#
# ~/.zshrc
#

# Disable Flow Control
stty -ixon

# Autoload zsh Functions
autoload -U compinit && compinit
autoload -U colors && colors
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
autoload -U vcs_info

# Set zsh Options
setopt correct
setopt extended_glob
setopt extended_history share_history
setopt hist_find_no_dups hist_ignore_dups hist_verify
setopt prompt_subst

# Source Completion Files
#
if [[ -n "$WORKON_HOME" ]] && (( $+commands[virtualenvwrapper.sh] )); then
  source "$commands[virtualenvwrapper.sh]"
fi

if [[ -s "$HOME/.config/git-prompt.sh" ]]; then
  source "$HOME/.config/git-prompt.sh"
fi
#source ~/.git-prompt.sh


# History Settings
#
HISTSIZE=10000
SAVEHIST=10000
HISTFILE="$HOME/.zhistory"

# Enhanced Tab Completion
#
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
# autocorrect
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:match:*' original only
# increase max-errors based on length of word
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
# kill
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#) ([0-9a-z-]#)*=$color[green]=0=$color[black]"
zstyle ':completion:*:*:kill:*' force-list always

# Prompt Settings
#
if [[ -s "$HOME/.config/zsh/prompt.zsh" ]]; then
  source "$HOME/.config/zsh/prompt.zsh"
  PROMPT='%(?..%B%F{red}exit %?%f%b
)'\
'%B%F{black}($(vcs_info && echo $vcs_info_msg_0_)%B%F{black})%b'\
'%F{black}[%B%F{yellow}%~%f%F{black}]%f%b'\
'%(!.%F{red}#%f.%F{blue}$%f) '
else
PROMPT='%{$fg[black]%}[%{$fg[blue]%}%m%{$fg_bold[yellow]%}%~'\
'%{$fg_no_bold[black]%}]%{$fg_bold[black]%}(%{%b%F{10}%}'\
'$(__git_ps1 "%s")%{%f$fg_bold[black]%})'\
'%{$fg_bold[yellow]%}$%{$reset_color%} '
fi
SPROMPT="Correct $fg_bold[red]%R$reset_color to $fg_bold[green]%r$reset_color [nyae]? "


# }}}
# Dynamic Window Title {{{
# -----------------------------------------------------------------------------

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


# Custom dircolors
#
if [[ -s "$HOME/.config/dircolors" ]]; then
  eval $(dircolors -b "$HOME/.config/dircolors")
fi

# }}}
# Custom Keybindings {{{
# -----------------------------------------------------------------------------

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

# }}}
# Custom Aliases {{{
# -----------------------------------------------------------------------------

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

source "$HOME/.config/zsh/extract.zsh"
