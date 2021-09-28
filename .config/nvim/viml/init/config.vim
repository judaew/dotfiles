syntax on
filetype off
filetype plugin indent on

set termguicolors
set background=dark
colorscheme molokai
let g:molokai_origin = 1
let g:rehash = 1

set expandtab
set tabstop=4
set shiftwidth=4
set mouse=a

set title
set number
set noswapfile
set splitright
set splitbelow
set hidden
set noshowmode
set ignorecase
set conceallevel=2

" TODO: Make a function that can highlight tabs or trailling whitespace by hotkey.
set list listchars=tab:\│\ ,extends:>,precedes:<,nbsp:+

if !isdirectory($HOME."/.cache/nvim")
    call mkdir($HOME."/.cache/nvim", "", 0750)
    call mkdir($HOME."/.cache/nvim/undo", "", 0700)
endif

" Enable persistent undo so that undo history persists across vim sessions
if has('persistent_undo')
  set undofile
  set undodir=~/.cache/nvim/undo
endif

" 80 chars/line
if exists('&colorcolumn')
  set colorcolumn=80
endif
