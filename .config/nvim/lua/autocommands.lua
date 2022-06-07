vim.cmd ([[
    augroup local_general
        autocmd!
        autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
        autocmd TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=150, on_visual=true}
    augroup end

    augroup local_git
        autocmd!
        autocmd FileType gitcommit setlocal spell spl=en_us tw=72 colorcolumn=72
    augroup end

    augroup local_files
        autocmd!
        autocmd FileType Dockerfile  setlocal expandtab tabstop=2 shiftwidth=2
        autocmd FileType markdown setlocal wrap
        autocmd Filetype asciidoctor setlocal textwidth=79
        autocmd Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
        autocmd Filetype vim setlocal expandtab tabstop=2 shiftwidth=2
        autocmd Filetype yaml setlocal expandtab tabstop=2 shiftwidth=2
    augroup end
]])
