" ~/.vimrc

set nocompatible
call pathogen#runtime_append_all_bundles()
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
set formatoptions+=qn1

" editing
set list listchars=tab:\ \ ,extends:$,precedes:^,trail:·
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

set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpg
set wildignore+=*.a,*.o,*.obj,*.so,*.hi
set wildignore+=*/.git/*

" backup
set dir=~/.vim/tmp
set backup backupdir=~/.vim/backup
set undofile undodir=~/.vim/undo

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

nnoremap <silent> <Space> :nohlsearch<Bar>:echo<cr>
nnoremap <F5> :!make<cr>

" save the current file as root
cmap w!! w !sudo tee % >/dev/null<cr>:e!<cr><cr>

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

" autocommands
au BufRead,BufNew ~/.mutt/temp/mutt* so ~/.mutt/mutt.vim
au FileType edp set commentstring=//\ %s

augroup vimrcEx
  autocmd!

  " Restore cursor position when reopening a file
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal g`\"" |
        \ endif
augroup END

if has('mouse')
  set mouse=a
endif
