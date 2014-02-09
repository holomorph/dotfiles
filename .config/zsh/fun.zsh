# ~/.config/zsh/fun.zsh

n() {
  $EDITOR "${@[@]/#/"$HOME/doc/notes/"}";
}

## add notes completion, thanks to Earnestly
compdef "_files -W ~/doc/notes -/" n

## add pdf completion for llpp
compdef "_pdf" llpp
