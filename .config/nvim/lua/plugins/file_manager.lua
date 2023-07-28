local M = {}

function M.config()
    -- disable netrw at the very start of your init.lua
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    require("nvim-tree").setup({
        hijack_cursor = true,
        view = {
            width = 32,
            side = "left",
        },
        renderer = {
            indent_markers = {
                enable = true
            },
        },
        diagnostics = {
            enable = true,
            show_on_dirs = false,
            show_on_open_dirs = true,
            icons = {
                hint = "H",
                info = "I",
                warning = "W",
                error = "E",
            }
        },
        git = {
            enable = false
        }
    })
end

local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

function M.keys()
    -- Keymaps
    map("<Leader>fm", ":NvimTreeToggle<CR>", "File manager (toggle)")

    -- Function keys
    map("<F2>",   ":NvimTreeToggle<CR>",         "File manager (toggle)")
    map("<S-F2>", ":NvimTreeFindFileToggle<CR>", "File manager (find current file)")
end

return M
