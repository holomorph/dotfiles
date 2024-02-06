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
compdef "_files -W ~/doc/notes" n

compdef "_files -g '*.docx'" docx2txt
compdef "_files -g '*.xlsx'" xlsx2csv

# vim: syn=sh
