local keymap = vim.api.nvim_set_keymap

local opts = { noremap = true, silent = true }

vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Modes
--   normal_mode = "n"
--   insert_mode = "i"
--   visual_mode = "v"
--   visual_block_mode = "x"
--   term_mode = "t"
--   command_mode = "c"

-- Remove newbie crutches
keymap("", "<Up>", "<Nop>", opts)
keymap("", "<Down>", "<Nop>", opts)
keymap("", "<Left>", "<Nop>", opts)
keymap("", "<Right>", "<Nop>", opts)

-- Close all but the current one
keymap("n", "<Leader>o", ":only<CR>", opts)


-- Some useful shortcuts for quickfix
keymap("n", "<C-n>", ":cn<CR>", opts)
keymap("n", "<C-m>", ":cp<CR>", opts)
keymap("n", "<Leader>a", ":cclose<CR>", opts)

-- Better split switching
keymap("", "<C-j>", "<C-W>j", opts)
keymap("", "<C-k>", "<C-W>k", opts)
keymap("", "<C-h>", "<C-W>h", opts)
keymap("", "<C-l>", "<C-W>l", opts)

-- Buffers
keymap("n", "]b", ":bnext<CR>", opts)
keymap("n", "[b", ":bprev<CR>", opts)
-- Tabs
keymap("n", "]t", "tabn<CR>", opts)
keymap("n", "[t", "tabp<CR>", opts)

-- Exit on 'jj'
keymap("i", "jj", "<Esc>", opts)

-- Map <C-l> (redraw screen) to also turn off search highlighting until the
-- next search
keymap("n", "<C-l>", ":nohl<CR><C-l>", opts)

-- Copy to clipoard
keymap("v", "<Leader>y", "\"+y", opts)
keymap("n", "<Leader>Y", "\"+yg_", opts)
keymap("n", "<Leader>y", "\"+y", opts)
keymap("n", "<Leader>yy", "\"+yy", opts)
-- Paste from clipboard
keymap("", "<Leader>p", "\"p", opts)
keymap("", "<Leader>P", "\"P", opts)
keymap("", "<Leader>p", "\"p", opts)
keymap("", "<Leader>P", "\"P", opts)
