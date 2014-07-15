" whitespace.vim

function whitespace#buffer_strip_trailing()
    %s/\s\+$//e
endfunction
