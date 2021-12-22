-- This comes first, because we have mappings that depend on leader
-- with a map leader it's possible to do extra key combinations
vim.g.mapleader = ','

-- Remove newbie crutches (temporarily)
vim.api.nvim_set_keymap('n', '<Up>', '', {noremap = true})
vim.api.nvim_set_keymap('n', '<Down>', '', {noremap = true})
vim.api.nvim_set_keymap('n', '<Left>', '', {noremap = true})
vim.api.nvim_set_keymap('n', '<Right>', '', {noremap = true})

-- Close all but the current one
vim.api.nvim_set_keymap('n', '<leader>o', ':only<CR>', {})

-- Some useful shortcuts for quickfix
vim.api.nvim_set_keymap('n', '<C-n>', ':cn<CR>', {})
vim.api.nvim_set_keymap('n', '<C-m>', ':cp<CR>', {})
vim.api.nvim_set_keymap('n', '<Leader>a', ':cclose<CR>', {})

-- Better split switching
vim.api.nvim_set_keymap('', '<C-j>', '<C-W>j', {})
vim.api.nvim_set_keymap('', '<C-k>', '<C-W>k', {})
vim.api.nvim_set_keymap('', '<C-h>', '<C-W>h', {})
vim.api.nvim_set_keymap('', '<C-l>', '<C-W>l', {})

-- Buffers
vim.api.nvim_set_keymap('n', ']b', ':bnext<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', '[b', ':bprev<CR>', {noremap = true})
-- Tabs
vim.api.nvim_set_keymap('n', ']t', ':tabn<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', '[t', ':tabp<CR>', {noremap = true})

-- Exit on 'jj'
vim.api.nvim_set_keymap('i', 'jj', '<Esc>', {})

-- Map <C-l> (redraw screen) to also turn off search highlighting until the
-- next search
vim.api.nvim_set_keymap('n', '<C-l>', ':nohl<CR><C-l>', {noremap = true})

-- Copy to clipboard
vim.api.nvim_set_keymap('v', '<Leader>y',  '\"+y', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>Y',  '\"+yg_', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>y',  '\"+y', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>yy', '\"+yy', {noremap = true})
-- Paste from clipboard
vim.api.nvim_set_keymap('n', '<Leader>p', '\"p', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>P', '\"P', {noremap = true})
vim.api.nvim_set_keymap('v', '<Leader>p', '\"p', {noremap = true})
vim.api.nvim_set_keymap('v', '<Leader>P', '\"P', {noremap = true})

-- Spell
vim.api.nvim_set_keymap('n', '<Leader>s', '', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>ss', ':setlocal spell spl=en_us,ru_yo<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>sn', ':setlocal nospell<CR>', {noremap = true})

