# ~/.config/bash/fun.sh

cd() {
  builtin cd "$@" && ls;
}
