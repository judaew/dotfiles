au FileType vim         setlocal expandtab tabstop=2 shiftwidth=2
au FileType go          setlocal noexpandtab tabstop=4 shiftwidth=4
au FileType asciidoctor setlocal textwidth=79
au FileType yaml        setlocal expandtab tabstop=2 shiftwidth=2
au FileType Dockerfile  setlocal expandtab tabstop=2 shiftwidth=2
au FileType gitcommit   setlocal spell spl=ru_yo,en_us tw=72 colorcolumn=72

" Highlight on yank --- Neovim now has a built-in function to briefly highlight
" the yanked region (similarly to vim-highlightedyank).
au TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=150, on_visual=true}
