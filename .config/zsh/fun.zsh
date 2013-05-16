# ~/.config/zsh/function.zsh

function cd() {
  builtin cd "$@" && ls;
}
