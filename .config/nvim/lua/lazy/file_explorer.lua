return {
    -- File Explorer
    {
        "nvim-tree/nvim-tree.lua",
        lazy = false,
        config = function()
            require("plugins.file_manager").config()
            require("plugins.file_manager").keys()
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
