local keymap = vim.keymap.set

-- Function keys
keymap("n", "<F4>", "<cmd>GV<CR>", { desc = "Commit browser" })
keymap("n", "<S-F4>", "<cmd>GV!<CR>", { desc = "Commit browser" })
