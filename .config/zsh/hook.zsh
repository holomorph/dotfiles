# zsh/hook.zsh

precmd_functions+=(vcs_info)

__print_title() {
  print -Pn "\e];%n@%m (%~) - Terminal\a"
}

chpwd() {
  ls
}

case $TERM in
  ( rxvt* | vte* | xterm* ) precmd_functions+=(__print_title) ;;
esac
