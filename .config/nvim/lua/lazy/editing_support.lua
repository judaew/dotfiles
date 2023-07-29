return {
    -- Comment plugin
    {
        "numToStr/Comment.nvim",
        keys = {
            { "gcc", mode = "n",          desc = "Comment current line" },
            { "gc",  mode = { "n", "o" }, desc = "Comment linewise" },
            { "gc",  mode = "x",          desc = "Comment linewise (visual)" },
            { "gbc", mode = "n",          desc = "Comment current block" },
            { "gb",  mode = { "n", "o" }, desc = "Comment blockwise" },
            { "gb",  mode = "x",          desc = "Comment blockwise (visual)" },
        },
        config = function()
            require("Comment").setup({mappings={basic=true, extra=false}})
        end
    },

    -- Multiple replacements
    {
        "AckslD/muren.nvim",
        cmd = { "MurenToggle", "MurenOpen", "MurenUnique" },
        keys = {
            {"<Leader>rm", function() require('muren.api').toggle_ui() end,
            desc="[M]ultiple [r]eplacements (toggle)"}
        },
        config = true
    },

    -- Automatic keyboard layout switching in insert mode
    {
        "lyokha/vim-xkbswitch",
        lazy = false,
        init = function()
            vim.g.XkbSwitchEnabled = 1
            vim.g.XkbSwitchLib = "/opt/local/lib/libInputSourceSwitcher.dylib"
        end
    },

    -- Detect and adjusts tabstop and shiftwidth automatically
    {
        "tpope/vim-sleuth",
        lazy = false
    },
}
