# zsh/hook.zsh

precmd_functions+=(vcs_info)

__print_title() {
  print -Pn "\e];%n@%m (%~) - Terminal\a"
}

__print_title_alacritty() {
  print -Pn "\e]0;%n@%m (%~) - Terminal\a"
}

chpwd() {
  ls
}

case $TERM in
  ( rxvt* | vte* | xterm* ) precmd_functions+=(__print_title) ;;
  ( alacritty ) precmd_functions+=(__print_title_alacritty) ;;
esac
