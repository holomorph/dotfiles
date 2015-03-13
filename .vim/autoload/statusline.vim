" Set the statusline
" Created: 10 March 2015

function! statusline#fugitive()
  let l:path = expand("%")
  if exists('*fugitive#statusline') && !empty(l:path) && !isdirectory(l:path)
    return fugitive#statusline()
  endif
  return ""
endfunction

function! statusline#syntastic()
  if exists('*SyntasticStatuslineFlag')
    return SyntasticStatuslineFlag()
  endif
  return ""
endfunction
