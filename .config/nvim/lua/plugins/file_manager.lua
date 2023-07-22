local key = require("utils/keymap")

local M = {}

function M.config()
    -- disable netrw at the very start of your init.lua
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    require("nvim-tree").setup({
        view = {
            width = 32,
        }
    })
end

function M.keys()
    -- Keymaps
    key.set("n", "<Leader>fm", ":NvimTreeToggle<CR>", "File manager (toggle)")

    -- Function keys
    key.set("n", "<F2>",   ":NvimTreeToggle<CR>", "File manager (toggle)")
    key.set("n", "<S-F2>", ":NvimTreeFindFileToggle<CR>", "File manager (find current file)")
end

return M
