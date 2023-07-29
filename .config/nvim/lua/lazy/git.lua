return {
    -- Git wrapper
    {
        "tpope/vim-fugitive",
        cmd = { "G", "Git" },
        init = function() require("utils.lazy").git_load("gv.vim") end,
        dependencies = {
            "tpope/vim-rhubarb" -- For enable :Gbrowse
        }
    },

    -- Another git wrapper
    {
        "NeogitOrg/neogit",
        enabled = false,
        dependencies = "nvim-lua/plenary.nvim",
        config = true
    },

    -- Git commit browser
    {
        "junegunn/gv.vim",
        lazy = true,
        keys = {
            {"<F4>", desc="Commit browser"},
            {"<S-F4>", desc="Commit browser"}
        },
        init = function() require("utils.lazy").git_load("gv.vim") end,
        config = function() require("plugins.gv").keys() end
    },

    -- Git signs
    {
        "lewis6991/gitsigns.nvim",
        lazy = true,
        init = function() require("utils.lazy").git_load("gitsigns.nvim") end,
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require("plugins.gitsigns").config() end
    },
}
