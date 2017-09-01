# bash/hook.bash
# zsh-like directory stack management

chpwd() {
  if [[ "$OLDPWD" != "$PWD" ]]; then
    ls
  fi
}

cd() {
  builtin pushd "$@" >/dev/null && chpwd;
}

pushd() {
  case "$@" in
    ( "" ) builtin pushd ~ && chpwd;;
    ( * ) builtin pushd "$@" && chpwd;;
  esac
}

popd() {
  builtin popd "$@" && chpwd;
}
