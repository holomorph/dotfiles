# ~/.bash_profile

# inputrc
export INPUTRC="$HOME/.config/bash/inputrc"

[[ -r "$HOME/.profile" ]] && . "$HOME/.profile"
[[ -n "$BASH" && -r "$HOME/.bashrc" ]] && . "$HOME/.bashrc"
