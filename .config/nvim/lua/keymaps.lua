local map = function(mode, keys, func, desc)
    if desc then
        vim.keymap.set(mode, keys, func, { desc=desc, noremap=true})
    else
        vim.keymap.set(mode, keys, func, { noremap=true})
    end
end

vim.g.mapleader = ","
vim.g.maplocalleader = ","

-- Modes
--   normal_mode       = "n"
--   insert_mode       = "i"
--   visual_mode       = "v"
--   visual_block_mode = "x"
--   term_mode         = "t"
--   command_mode      = "c"

-- Use arrows for move and resize (with shift) to windows
map("n", "<Up>", "<C-w>k")
map("n", "<Down>", "<C-w>j")
map("n", "<Left>", "<C-w>h")
map("n", "<Right>", "<C-w>l")
map("n", "<S-Up>", "<C-w>+")
map("n", "<S-Down>", "<C-w>-")
map("n", "<S-Left>", "<C-w>>")
map("n", "<S-Right>", "<C-w><")

-- Close all but the current one
map("n", "<Leader>b", ":only<CR>")

-- Some useful shortcuts for quickfix
map("n", "<C-n>", ":cn<CR>")
map("n", "<C-m>", ":cp<CR>")

-- Better split switching
map("", "<Leader>j", "<C-W>j")
map("", "<Leader>k", "<C-W>k")
map("", "<Leader>h", "<C-W>h")
map("", "<Leader>l", "<C-W>l")

-- Buffers
map("n", "]b", ":bnext<CR>")
map("n", "[b", ":bprev<CR>")
-- Tabs
map("n", "]t", ":tabn<CR>")
map("n", "[t", ":tabp<CR>")

-- Exit on "jj"
map("i", "jj", "<Esc>")

-- Map <C-l> (redraw screen) to also turn off search highlighting until the
-- next search
map("n", "<C-l>", ":nohl<CR><C-l>")

-- Copy to clipoard
map("n", "<Leader>Y", "\"+yg_")
map("n", "<Leader>yy", "\"+yy")
map("v", "<Leader>y", "\"+y")
-- Paste from clipboard
map("", "<Leader>p", "\"p")
map("", "<Leader>P", "\"P")

-- Toggle for wrap options
map("n", "<Leader>ww", ":set wrap!<CR>")

-- Swap lines, see https://vim.fandom.com/wiki/Moving_lines_up_or_down
map("n", "<Leader>a", ":m .+1<CR>==")
map("n", "<Leader>A", ":m .-2<CR>==")
map("v", "<Leader>a", ":m '>+1<CR>gv=gv")
map("v", "<Leader>A", ":m '<-2<CR>gv=gv")

-- Diagnostic keymaps
local diag = vim.diagnostic
map("n", "[d",        diag.goto_prev,  "Go to previous diagnostic message")
map("n", "]d",        diag.goto_next,  "Go to next diagnostic message")
map("n", "<Leader>e", diag.open_float, "Open floating diagnostic message")
map("n", "<Leader>q", diag.setloclist, "Open diagnostics list")

-- Show all the items at a given buffer position
-- (It's useful for debug colorscheme)
-- map("n", "<Leader>i", vim.show_pos)
