# my "sunaku" prompt for ZSH using vcs_info stdlib
# http://snk.tuxfamily.org/log/sunaku-zsh-prompt.png
# https://github.com/sunaku/home/blob/master/.zsh/config/prompt.zsh

PROMPT='%(?..%B%F{red}exit %?%f%b
)%B%F{black}[%b%f'\
'$(vcs_info && echo "$vcs_info_msg_0_")'\
'%F{11}%~%B%F{black}]%b%f'\
'%(!.%F{red}#%f.%F{blue}$%f) '

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr     '%B%F{green}+%f%b'
zstyle ':vcs_info:*' unstagedstr   '%B%F{yellow}*%f%b'
zstyle ':vcs_info:*' formats       '%F{9}%b%f%c%u%m '
zstyle ':vcs_info:*' actionformats '%F{9}%b%f%c%u%m %F{14}%a%f '
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-aheadbehind git-remotebranch git-stash

### git: Show marker if there are untracked files in repository
# Make sure staged is in 'formats':  %c
+vi-git-untracked() {
  # This will show the marker if there are any untracked files in repo.
  if [[ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" == 'true' ]]; then
    git status --porcelain | grep -L '^??' &>/dev/null && \
      hook_com[unstaged]+='%B%F{white}^%b%f'
  fi
}

### git: Show stash existence
# Make sure misc is in 'formats':  %m
+vi-git-stash() {
  if [[ -s ${hook_com[base]}/.git/refs/stash ]]; then
    hook_com[misc]+="%B%F{14}#%b%f"
  fi
}

### git: Show +N-M when the local branch is N ahead or M behind remote HEAD.
# Make sure misc is in 'formats':  %m
+vi-git-aheadbehind() {
  local ahead behind
  local -a gitstatus

  ahead=$(git rev-list --count ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null)
  behind=$(git rev-list --count HEAD..${hook_com[branch]}@{upstream} 2>/dev/null)
  (( $ahead )) && gitstatus+=( "%F{10}+${ahead}%f" )
  (( $behind )) && gitstatus+=( "%F{red}-${behind}%f" )

  hook_com[misc]+=${(j::)gitstatus}
}

### git: Show remote branch name for remote-tracking branches
# Make sure branch is in 'formats':  %b
+vi-git-remotebranch() {
  # Are we on a remote-tracking branch?
  local remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
      --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

  # The first test will show a tracking branch whenever there is one. The
  # second test, however, will only show the remote branch's name if it
  # differs from the local one.
  # if [[ -n ${remote} ]] ; then
  if [[ -n ${remote} && ${remote#*/} != ${hook_com[branch]} ]]; then
    hook_com[branch]="${hook_com[branch]}%F{13}(${remote})%f"
  fi
}
