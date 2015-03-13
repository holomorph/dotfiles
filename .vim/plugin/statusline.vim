" statusline.vim - Set the statusline
" Created: 10 March 2015

if exists("g:loaded_statusline") || &cp
  finish
endif
let g:loaded_statusline = 1

function! s:statusline()
  let s:stl = "%f\ %<%y%m%r%="
  let s:stl .= "%{statusline#syntastic()}"
  let s:stl .= "%12.(\(%l,%c%V\)%)\ %P"
  let &stl = s:stl
endfunction

call s:statusline()
