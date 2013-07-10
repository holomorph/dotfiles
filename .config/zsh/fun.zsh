# ~/.config/zsh/fun.zsh

cd() {
  builtin cd "$@" && ls;
}

n() {
  $EDITOR "${@[@]/#/"$HOME/doc/notes/"}";
}
compdef "_files -W ~/doc/notes -/" n
