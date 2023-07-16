local key = require("utils/keymap")

local chadtree_settings = {
    view = { width = 32 },
    theme = {
        icon_glyph_set = "devicons",
        text_colour_set = "env",
        icon_colour_set = "none"
    }
}
vim.api.nvim_set_var("chadtree_settings", chadtree_settings)

-- Keymaps
key.set("n", "<Leader>fm", ":CHADopen<CR>", "File manager (toggle)")

-- Function keys
key.set("n", "<F2>",   ":CHADopen<CR>", "File manager (toggle)")
key.set("n", "<S-F2>", ":CHADopen --version-ctl<CR>", "File manager at VCS top level (toggle)")
