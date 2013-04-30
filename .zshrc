# ~/.zshrc

autoload -U compinit && compinit
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search

setopt correct
setopt extended_glob
setopt extended_history share_history
setopt hist_find_no_dups hist_ignore_dups hist_verify
setopt hist_expire_dups_first
setopt prompt_subst
unsetopt flow_control

HISTSIZE=20000
SAVEHIST=10000
HISTFILE="$HOME/.logs/zhistory"

## configs
for cfg in aliases bindings extract zstyle; do
	[[ -e "$HOME/.config/zsh/$cfg.zsh" ]] && \
		source "$HOME/.config/zsh/$cfg.zsh"
done
unset cfg

## prompt
if [[ -s "$HOME/.config/zsh/prompt.zsh" ]]; then
	source "$HOME/.config/zsh/prompt.zsh"
else
	PROMPT="%(?..%B%F{red}exit %?%f%b
)%B%F{black}[%b%f"\
'%F{11}%~%B%F{black}]%b%f'\
'%(!.%F{red}#%f.%F{blue}$%f) '
fi
SPROMPT="Correct %B%F{red}%R%b%f to %B%F{green}%r%b%f [nyae]? "

## envoy
envoy -t gpg-agent
source <(envoy -p)
