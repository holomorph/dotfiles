" xdg.vim - put some files in less crazy places
" Created:     25 February 2014
" Commentary:  Vim does not bother trying to mkdir directories configured in
" vimrc.  How annoying!

if exists("g:loaded_xdg") || &cp
    finish
endif
let g:loaded_xdg = 1

" FindDataHome:  Check if "$XDG_DATA_HOME" is set and use it, otherwise
" default to "~/.local/share/vim" for Vim's data directory.  Return the
" directory name.
function! s:FindDataHome()
  if exists("$XDG_DATA_HOME")
    return $XDG_DATA_HOME . "/vim"
  else
    return expand(expand("~/.local/share/vim"))
  endif
endfunction

" SetDataSettings:  Take the Vim data home directory and set the swap, backup,
" and undo file directories, as well as setting the location for viminfo.
function! s:SetDataSettings(dir)
  exe "set dir="              . a:dir . "/tmp"
  exe "set backup backupdir=" . a:dir . "/backup"
  exe "set undofile undodir=" . a:dir . "/undo"
  exe "set viminfo+=n"        . a:dir . "/viminfo"
endfunction

" InitXDG:  Create the directories and set the corresponding directory options
function! s:InitXDG(dir, perm)
  sil! call mkdir(a:dir, "p", a:perm)
  sil! call mkdir(a:dir . "/tmp", a:perm)
  sil! call mkdir(a:dir . "/backup", a:perm)
  sil! call mkdir(a:dir . "/undo", a:perm)

  call s:SetDataSettings(a:dir)
endfunction

call s:InitXDG(s:FindDataHome(), 0700)
