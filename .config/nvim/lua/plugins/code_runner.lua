require('sniprun').setup({
    binary_path="/opt/local/sbin/sniprun",
})

local keymap = vim.keymap.set

-- Function keys
keymap({"n", "v"}, "<F7>", ":SnipRun<CR>", { desc = "Code runner: Run the lines" })
keymap("n", "<S-F7>", ":%SnipRun<CR>", { desc = "Code runner: Run the buffer" })
keymap("n", "<F8>", ":SnipClose<CR>", { desc = "Code runner: Close" })
keymap("n", "<S-F8>", ":SnipReset<CR>", { desc = "Code runner: Reset" })
