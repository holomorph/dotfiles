# zsh/fun.zsh

n() {
    local dir="$HOME/doc/notes"
    local editor="${EDITOR:-vim}" # likely vim anyways

    # try emacsclient if arglist includes org or encrypted files
    if [[ "$@" =~ '(.gpg|.org)' ]]; then
        editor=(emacsclient -t -avim)
    fi
    command "${editor[@]}" "${@[@]/#/$dir/}";
}

# add notes completion, thanks to Earnestly
compdef "_files -W ~/doc/notes -/" n

# moar
compdef "_files -g '*.(pdf|xps|zip|cbz|jpg)'" llpp

# vim: syn=sh
