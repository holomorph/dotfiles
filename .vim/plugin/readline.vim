" readline.vim - less annoying command mode
" Created:     04 November 2014
" Commentary:  Derived from tpope's rsi plugin here:
" <https://github.com/tpope/vim-rsi>, with smaller scope (limited to command
" mode).

if exists("g:loaded_readline") || &cp
    finish
endif
let g:loaded_readline = 1

set ttimeout
if &ttimeoutlen == -1
    set ttimeoutlen=50
endif

cnoremap <C-a> <home>
cnoremap <C-b> <left>
cnoremap <C-f> <right>
cnoremap <C-d> <delete>

cnoremap <M-b> <S-left>
cnoremap <M-f> <S-right>
cnoremap <M-n> <down>
cnoremap <M-p> <up>
cnoremap <M-backspace> <C-w>

if !has("gui_running")
    sil! exe "set <S-left>=\<esc>b"
    sil! exe "set <S-right>=\<esc>f"
    sil! exe "set <F32>=\<esc>n"
    sil! exe "set <F33>=\<esc>p"
    sil! exe "set <F34>=\<esc>\<C-?>"
    cmap <F32> <M-n>
    cmap <F33> <M-p>
    cmap <F34> <M-backspace>
endif
