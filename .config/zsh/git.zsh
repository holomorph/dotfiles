# zsh/git.zsh

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable hg git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr     '%B%F{green}+%f%b'
zstyle ':vcs_info:*' unstagedstr   '%B%F{yellow}*%f%b'
zstyle ':vcs_info:*' formats       '%F{9}%b%f%c%u%m '
zstyle ':vcs_info:*' actionformats '%F{9}%b%f%c%u %F{14}%a%f '
zstyle ':vcs_info:git*+set-message:*' hooks git-stash

# show stash existence (%m)
+vi-git-stash() {
    if [[ -s ${hook_com[base]}/.git/refs/stash ]]; then
        hook_com[misc]+="%B%F{14}#%b%f"
    fi
}
