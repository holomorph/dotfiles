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
zstyle ':vcs_info:*' formats       '%c%u%F{9}%b%f%m '
zstyle ':vcs_info:*' actionformats '%c%u%F{9}%b%f%m %F{14}%a%f '
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-aheadbehind git-remotebranch

### git: Show marker if there are untracked files in repository
# Make sure you have added staged to your 'formats':  %c
+vi-git-untracked() {
  if git status --porcelain | grep '??' &>/dev/null; then
  # This will show the marker if there are any untracked files in repo.
  # If instead you want to show the marker only if there are untracked
  # files in $PWD, use:
  # if [[ -n $(git ls-files --others --exclude-standard) ]]; then
    hook_com[unstaged]+='%B%F{white}^%b%f'
  fi
}

### git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
# Make sure you have added misc to your 'formats':  %m
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
# Make sure you have added staged to your 'formats':  %b
+vi-git-remotebranch() {
  local remote

  # Are we on a remote-tracking branch?
  remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
      --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

  # The first test will show a tracking branch whenever there is one. The
  # second test, however, will only show the remote branch's name if it
  # differs from the local one.
  # if [[ -n ${remote} ]] ; then
  if [[ -n ${remote} && ${remote#*/} != ${hook_com[branch]} ]]; then
    hook_com[branch]="${hook_com[branch]}%F{cyan}(${remote})%f"
  fi
}
