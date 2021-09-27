-- Open project (pwd)
vim.api.nvim_set_keymap('n', '<Leader>n', ':Fern . -drawer<CR>', {})
-- Open current working directory
vim.api.nvim_set_keymap('n', '<Leader>f', ':Fern . -reveal=% -drawer<CR>', {})
