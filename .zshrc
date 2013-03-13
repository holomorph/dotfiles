# ~/.zshrc

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
HISTFILE="$HOME/.logs/zhistory"

# Config Files
#
for cfg in aliases bindings extract zstyle; do
  [[ -e "$HOME/.config/zsh/$cfg.zsh" ]] && \
		source "$HOME/.config/zsh/$cfg.zsh"
done

# Dircolors
#
[[ -e "$HOME/.config/dircolors" ]] && \
  eval $(dircolors -b "$HOME/.config/dircolors")

# Prompt Settings
#
if [[ -s "$HOME/.config/zsh/prompt.zsh" ]]; then
  source "$HOME/.config/zsh/prompt.zsh"
else
  PROMPT='%(?..%B%F{red}exit %?%f%b
)'\
'%F{black}[%F{11}%~%F{black}]%f'\
'%(!.%F{red}#%f.%F{blue}$%f) '
fi
SPROMPT="Correct %B%F{red}%R%b%f to %B%F{green}%r%b%f [nyae]? "

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
