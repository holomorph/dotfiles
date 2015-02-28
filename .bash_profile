# ~/.bash_profile

mkdir -p "$HOME/.cache/bash"
[[ -r "$HOME/.profile" ]] && . "$HOME/.profile"
[[ -n "$BASH" && -r "$HOME/.bashrc" ]] && . "$HOME/.bashrc"
