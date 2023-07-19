vim.g.coq_settings = {
    auto_start = true,
    -- Short names:
    -- * LSP            | * REG  = registers  | * TAG = tags
    -- * BUF  = buffers | * SNIP = snippets   | * TMUX
    -- * PATH = paths   | * T9   = tabnine    | * TS = Tree Sitter
    clients = {
        lsp = {
            enabled = true,
            short_name = "LSP"
        },
        buffers = { enabled = true },
        paths = { enabled = true },
        registers = { enabled = true },
        snippets = { enabled = true },
        tabnine = { enabled = false },
        tags = { enabled = true },
        third_party = { enabled = false },
        tmux = { enabled = true },
        tree_sitter = { enabled = true },
    },
    display = {
        icons = { mode = "none" },
        statusline = { helo = false }
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
