-- local cmp = require"cmp"
--
vim.g.coq_settings = {
    auto_start = true,
    clients = {
        lsp = { enabled = true },
        tree_sitter = { enabled = true },
        tabnine = { enabled = false },
    },
    display = {
        icons = { mode = "none" }
    },
    keymap = {
        recommended = false,
        manual_complete = '<c-n>'
    }
}

-- See https://github.com/ms-jpq/coq_nvim/blob/coq/docs/KEYBIND.md#custom-keybindings
vim.cmd([[
ino <silent><expr> <Esc>   pumvisible() ? "\<C-e><Esc>" : "\<Esc>"
ino <silent><expr> <C-c>   pumvisible() ? "\<C-e><C-c>" : "\<C-c>"
ino <silent><expr> <BS>    pumvisible() ? "\<C-e><BS>"  : "\<BS>"
ino <silent><expr> <CR>    pumvisible() ? (complete_info().selected == -1 ? "\<C-e><CR>" : "\<C-y>") : "\<CR>"
" ino <silent><expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
" ino <silent><expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<BS>"
]])

