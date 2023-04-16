-- Auto Commands
vim.cmd ([[
    augroup local_general
        autocmd!
        autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
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

-- Highlight on yank.
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight',
    { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*'
})

-- Custom Commands
vim.cmd ([[
command! -bar SpellEn set spell spelllang=en spellfile=${HOME}/.local/share/nvim/site/spell/en.utf-8.add
command! -bar SpellUk set spell spelllang=uk spellfile=${HOME}/.local/share/nvim/site/spell/uk.utf-8.add
command! -bar SpellRu set spell spelllang=ru_yo spellfile=${HOME}/.local/share/nvim/site/spell/ru.utf-8.add
command! -bar NoSpell set nospell
]])
