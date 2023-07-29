return {
    -- Fuzzy Finder
    {
        "nvim-telescope/telescope.nvim",
        lazy = false,
        dependencies = {
            { "nvim-lua/plenary.nvim" },
            {
                "nvim-telescope/telescope-fzf-native.nvim",
                build = "make",
                cond = function() return vim.fn.executable "make" == 1 end,
                dependencies = { "nvim-telescope/telescope.nvim" },
                config = function() require("plugins.telescope").fzf_native() end
            },
        },
        -- See also "plugins/ide/lsp.lua"
        config = function()
            require("plugins.telescope").telescope()
            require("plugins.telescope").telescope_keys()
        end
    },

    -- Another fuzzy Finder
    {
        "junegunn/fzf.vim",
        lazy = false,
        dependencies = { "junegunn/fzf" }
    },
}
