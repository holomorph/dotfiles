" whitespace.vim

if exists("g:loaded_whitespace") || &cp
    finish
endif
let g:loaded_whitespace = 1

function whitespace#buffer_strip_trailing()
    %s/\s\+$//e
endfunction
