" Set the statusline
" Created: 10 March 2015

function! statusline#syntastic()
  if exists('*SyntasticStatuslineFlag')
    return SyntasticStatuslineFlag()
  endif
  return ""
endfunction
