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

# git completion
# http://www.zsh.org/mla/workers/2011/msg00490.html
#__git_files () { 
#    _wanted files expl 'local files' _files     
#}

#
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

# }}}
# Prompt Settings {{{
# -----------------------------------------------------------------------------

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
PROMPT='%{$fg[black]%}┌─[%{$fg_bold[yellow]%}%~%{$reset_color$fg[black]%}]$(__git_ps1 "\e[0;30m[\e[1;31m%s\e[0;30m]")
└─╼%{$reset_color%} '

#PROMPT='%{$fg[black]%}[%(!.%{$fg[red]%}.%{$fg[white]%})%n%{$fg[white]%}@%{$fg[blue]%}%m%{$fg_bold[yellow]%}%~%{$reset_color$fg[black]%}]%(!.%{$fg[red]%}#.%{$fg[blue]%}$)%{$reset_color%} '
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
#alias tm="urxvtc -name chatmail -e tmux attach-session -d -t 0"
#alias usbmount="sudo ntfs-3g -o gid=100,fmask=113,dmask=002 /dev/sde1 /mnt/usb"
#alias usbumount="sudo umount /mnt/usb"

# }}}
# Function: Extract {{{
# from: https://github.com/sorin-ionescu/prezto/blob/master/modules/archive/functions/extract)
# -----------------------------------------------------------------------------

function extract() {
  local remove_archive
  local success
  local file_name
  local extract_dir

  if (( $# == 0 )); then
echo "Usage: extract [-option] [file ...]"
    echo
echo Options:
    echo " -r, --remove Remove archive."
    echo
echo "Report bugs to <sorin.ionescu@gmail.com>."
  fi

remove_archive=1
  if [[ "$1" == "-r" ]] || [[ "$1" == "--remove" ]]; then
remove_archive=0
    shift
fi

while (( $# > 0 )); do
if [[ ! -f "$1" ]]; then
echo "extract: '$1' is not a valid file" 1>&2
      shift
continue
fi

success=0
    file_name="$( basename "$1" )"
    extract_dir="$( echo "$file_name" | sed "s/\.${1##*.}//g" )"
    case "$1" in
      (*.tar.gz|*.tgz) tar xvzf "$1" ;;
      (*.tar.bz2|*.tbz|*.tbz2) tar xvjf "$1" ;;
      (*.tar.xz|*.txz) tar --xz --help &> /dev/null \
        && tar --xz -xvf "$1" \
        || xzcat "$1" | tar xvf - ;;
      (*.tar.zma|*.tlz) tar --lzma --help &> /dev/null \
        && tar --lzma -xvf "$1" \
        || lzcat "$1" | tar xvf - ;;
      (*.tar) tar xvf "$1" ;;
      (*.gz) gunzip "$1" ;;
      (*.bz2) bunzip2 "$1" ;;
      (*.xz) unxz "$1" ;;
      (*.lzma) unlzma "$1" ;;
      (*.Z) uncompress "$1" ;;
      (*.zip) unzip "$1" -d $extract_dir ;;
      (*.rar) unrar e -ad "$1" ;;
      (*.7z) 7za x "$1" ;;
      (*.deb)
        mkdir -p "$extract_dir/control"
        mkdir -p "$extract_dir/data"
        cd "$extract_dir"; ar vx "../${1}" > /dev/null
        cd control; tar xzvf ../control.tar.gz
        cd ../data; tar xzvf ../data.tar.gz
        cd ..; rm *.tar.gz debian-binary
        cd ..
        ;;
      (*.chm) extract_chmLib "$1" $extract_dir ;;
      (*)
        echo "extract: '$1' cannot be extracted" 1>&2
        success=1
        ;;
    esac

    (( success = $success > 0 ? $success : $? ))
    (( $success == 0 )) && (( $remove_archive == 0 )) && rm "$1"
    shift
done
}

# }}}
