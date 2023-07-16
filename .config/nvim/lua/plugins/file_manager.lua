local chadtree_settings = {
    view = { width = 32 },
    theme = {
        icon_glyph_set = "devicons",
        text_colour_set = "env",
        icon_colour_set = "none"
    }
}
vim.api.nvim_set_var("chadtree_settings", chadtree_settings)

local keymap = vim.keymap.set
keymap("n", "<Leader>fm", ":CHADopen<CR>", { desc = "File manager (toggle)", noremap = true, silent = true })

-- Function keys
keymap("n", "<F2>", ":CHADopen<CR>",
    { desc = "File manager (toggle)", noremap = true, silent = true })
keymap("n", "<S-F2>", ":CHADopen --version-ctl<CR>",
    { desc = "File manager at VCS top level (toggle)", noremap = true, silent = true })
