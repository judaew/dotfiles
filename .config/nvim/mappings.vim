" This comes first, because we have mappings that depend on leader with a map
" leader it's possible to do extra key combinations
let mapleader = ","

" Remove newbie crutches
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" Close all but the current one
nmap <Leader>o :only<CR>

" Some useful shortcuts for quickfix
nmap <C-n> :cn<CR>
nmap <C-m> :cp<CR>
nmap <Leader>a :cclose<CR>

" Better split switching
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Buffers
nmap ]b :bnext<CR>
nmap [b :bprev<CR>
" Tabs
nmap ]t :tabn<CR>
nmap [t :tabp<CR>

" Exit on 'jj'
imap jj <Esc>

" Map <C-l> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-l> :nohl<CR><C-l>

" Copy to clipoard
vmap <Leader>y  "+y
nmap <Leader>Y  "+yg_
nmap <Leader>y  "+y
nmap <Leader>yy "+yy
" Paste from clipboard
nmap <Leader>p "p
nmap <Leader>P "P
vmap <Leader>p "p
vmap <Leader>P "P

" Spell
nmap <Leader>s <Nop>
nmap <Leader>ss :setlocal spell spl=en_us,ru_yo<CR>
nmap <Leader>sn :setlocal nospell<CR>
