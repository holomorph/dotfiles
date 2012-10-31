"
" file:   ~/.vimrc
"
" author: Simon Gomizelj
" alias:  vodik
" site: https://github.com/vodik

set nocompatible
filetype indent plugin on
syntax on
colorscheme zenburn

" searching
set hlsearch
set incsearch
set smartcase
set ignorecase
set wrapscan

" formatting
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smartindent
set cinoptions=(0
set smartindent
set linebreak
set nolist
"set nowrap
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
set shortmess=at
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

" sane j k actions
nnoremap j gj
nnoremap k gk

" semicolon
map ; :
noremap ;; ;

if has('mouse')
  set mouse=a
endif

if has ('gui_running')
  set guioptions=acM
  set mousefocus
  set guifont=Cousine\ 8
  autocmd GUIEnter * set t_vb=
endif

