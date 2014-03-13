" xdg.vim - put some files in less crazy places
" Created:     25 February 2014
" Commentary:  Vim does not bother trying to mkdir directories configured in
" vimrc.  How annoying!

if exists("g:loaded_xdg") || &cp
    finish
endif
let g:loaded_xdg = 1

" GetUserDir:  Check if "env" is set and use it, otherwise default to
" "default" for the user directory.  Return the directory name.
function! s:GetUserDir(env, default)
  if exists(a:env)
    return a:env
  else
    return expand(expand(a:default))
  endif
endfunction

" SetCacheSettings:  Take the Vim cache home directory and set the swap,
" backup, and undo file directories, as well as setting the location for
" viminfo.
function! s:SetCacheSettings(dir)
  exe "set dir="              . a:dir . "/tmp"
  exe "set backup backupdir=" . a:dir . "/backup"
  exe "set undofile undodir=" . a:dir . "/undo"
  exe "set viminfo+=n"        . a:dir . "/viminfo"
endfunction

" InitXDG:  Create the XDG directories with "perm" and set the corresponding
" directory options
function! s:InitXDG(perm)
  let l:cache = s:GetUserDir("XDG_CACHE_HOME", "~/.cache") . "/vim"

  sil! call mkdir(l:cache, "p", a:perm)
  sil! call mkdir(l:cache . "/tmp", a:perm)
  sil! call mkdir(l:cache . "/backup", a:perm)
  sil! call mkdir(l:cache . "/undo", a:perm)

  call s:SetCacheSettings(l:cache)
endfunction

call s:InitXDG(0700)
