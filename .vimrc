" ~/.vimrc

set nocompatible
filetype indent plugin on
syntax on
let g:zenburn_old_Visual = 1
let g:zenburn_alternate_Visual = 1
let g:zenburn_high_Contrast = 1
colorscheme zenburn

" searching
set hlsearch
set incsearch
set smartcase
set ignorecase
set wrapscan

" formatting
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smartindent
set cinoptions=(0
set smartindent
set linebreak
set nolist
set nowrap
set formatoptions=qn1

" editing
set list listchars=tab:\ \ ,extends:$,precedes:^,trail:-
set nrformats+=alpha

" vim ui
set autoread
set autowrite
set showcmd
set title
set ruler
set cursorline
set hidden
set laststatus=2
set scrolloff=3
set wildmenu wildmode=list:longest,full
set completeopt=longest,menuone
set whichwrap+=<,>,[,],h,l
set history=50
set confirm
set equalalways
set shortmess=atToOI

set wildignore+=*.aux,*.out,*.toc             " LaTeX intermediates
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpg " images
set wildignore+=*.a,*.o,*.obj,*.so,*.hi
set wildignore+=*/.git/*                      " git repositories

" backup
set dir=~/.vim/tmp
set backup	  backupdir=~/.vim/backup
set undofile  undodir=~/.vim/undo

" fix typos
command! Q q
command! W w
command! Qa qa
command! Wq wq
command! Wa wa
command! Wqa wqa

nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>

" semicolon
map ; :
noremap ;; ;

" tap enter again to remove hlsearch
nnoremap <cr> :nohlsearch<cr>

" save the current file as root
cmap w!! w !sudo tee % >/dev/null<cr>:e!<cr><cr>

" sane j k actions
"nnoremap j gj
"nnoremap k gk

" pane movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" pane splitting
"set winwidth=84
"set winheight=5
"set winminheight=5
"set winheight=999

" autocommands {{{
augroup vimrcEx
  autocmd!

  autocmd BufRead * call SetStatusLine()
  autocmd BufWritePre * call Mkdir()

  " Restore cursor position when reopening a file
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal g`\"" |
        \ endif

  " when an omnicompletion opens up a preview window (eclim) the following
  " will close the window on cursor movement or insert-exit
  autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
  autocmd InsertLeave  * if pumvisible() == 0|pclose|endif
augroup END
" }}}


" Called when opening every file. If the containing directory doesn't
" exist, create it.
function! Mkdir()
  let dir = expand('%:p:h')
  if !isdirectory(dir)
    call mkdir(dir, "p")
    echo "created non-existing directory: " . dir
  endif
endfunction

function! SetStatusLine()
  let l:s1="%3.3n\\ %f\\ %h%m%r%w"
  let l:s2="[%{strlen(&filetype)?&filetype:'?'},\\ %{&encoding},\\ %{&fileformat}]"
  let l:s3="%=\\ 0x%-8B\\ \\ %-14.(%l,%c%V%)\\ %<%P"
  execute "set statusline=" . l:s1 . l:s2 . l:s3
endfunction

if has('mouse')
  set mouse=a
endif

" gvim
if has ('gui_running')
  set guioptions=acM
  set mousefocus
  set guifont=Consolas\ 10
  autocmd GUIEnter * set t_vb=
endif
