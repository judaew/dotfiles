" ###########################
" ### NVIMPAGER INIT FILE ###
" ###########################

" Options
" ###########################

set mouse=a
set title

" Mappings
" ###########################

let mapleader = ","

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

" Autocommands
" ###########################

augroup local_nvimpager
  autocmd!
  autocmd TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=150, on_visual=true}
augroup end
