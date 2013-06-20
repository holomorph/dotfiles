" ~/.vimrc

set nocompatible
call pathogen#runtime_append_all_bundles()
filetype plugin indent on
syntax on
let g:zenburn_old_Visual = 1
let g:zenburn_alternate_Visual = 1
let g:zenburn_high_Contrast = 1
colorscheme zenburn

" searching
set hlsearch
set ignorecase
set incsearch
set smartcase
set wrapscan

" formatting
set cinoptions=(0
set formatoptions+=qn1
set nolist
set nowrap
set shiftwidth=4
set softtabstop=2
set tabstop=4

" editing
set list listchars=tab:›\ ,extends:$,precedes:^,trail:·
set nrformats+=alpha

" folding
set foldenable
set foldmethod=marker
set foldmarker={{{,}}}
set foldcolumn=0

" vim ui
set autoread
set autowrite
set completeopt=longest,menuone,preview
set cursorline
set equalalways
set hidden
set history=50
set lazyredraw
set laststatus=2
set mouse=a
set ruler
set scrolloff=3
set shortmess=atToOI
set showcmd
set title
set whichwrap+=<,>,[,],h,l
set wildmenu wildmode=list:longest,full

set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpg
set wildignore+=*.a,*.o,*.obj,*.so,*.hi
set wildignore+=.git,.hg

" backup
set dir=~/.vim/tmp
set backup backupdir=~/.vim/backup
set undofile undodir=~/.vim/undo
set viminfo+=n~/.vim/.viminfo

" fix typos
command! Q q
command! W w
command! Qa qa
command! Wq wq
command! Wa wa
command! Wqa wqa

" nnoremap q: <Nop>
" nnoremap q/ <Nop>
" nnoremap q? <Nop>
nnoremap Q <Nop>

map ; :
noremap Y y$
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<cr>
nnoremap <F5> :!make<cr>
cmap w!! w !sudo tee % >/dev/null<cr>:e!<cr><cr>

" pane movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" fugitive
nnoremap <leader>gg :copen<cr>:Ggrep
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>ge :Gedit<cr>
nnoremap <silent> <leader>gb :Gblame<cr>
nnoremap <silent> <leader>gl :Glog<cr>

" autocommands
au BufRead,BufNew ~/.mutt/temp/mutt* so ~/.mutt/mutt.vim
au FileType edp set commentstring=//\ %s
au BufReadPost *
	\ if line("'\"") > 0 && line("'\"") <= line("$") |
	\ exe "normal g`\"" | endif
