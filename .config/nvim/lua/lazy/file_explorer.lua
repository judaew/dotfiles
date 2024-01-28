local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

return {
    -- File Explorer
    {
        "nvim-tree/nvim-tree.lua",
        lazy = false,
        keys = function()
            -- Keymaps
            map("<Leader>fm", ":NvimTreeToggle<CR>", "File manager (toggle)")

            -- Function keys
            map("<F2>",   ":NvimTreeToggle<CR>",         "File manager (toggle)")
            map("<S-F2>", ":NvimTreeFindFileToggle<CR>", "File manager (find current file)")
        end,
        config = function()
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
                },
                actions = {
                    change_dir = {
                        enable = true,
                        global = true,
                        restrict_above_cwd = false,
                    }
                }
            })
        end
    },

    -- Go to Terminal or File manager
    {
        "justinmk/vim-gtfo",
        keys = {
            {"gof", desc="$open: File manager (current file)"},
            {"goF", desc="$open: File manager (working directory)"},
            {"got", desc="$open: Terminal (current file)"},
            {"goT", desc="$open: Terminal (working directory)"}
        },
        config = function() vim.cmd([[
            let g:gtfo#terminals = { "mac": "kitty" }
        ]]) end
    },
}
