return {
    -- Dashboard / Startpage
    {
        "goolord/alpha-nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        event = "VimEnter",
        config = function() require("plugins.alpha-nvim").config() end
    },

    -- Improve the default vim.ui interfaces
    {
        "stevearc/dressing.nvim",
        event = "VeryLazy",
        opts = {}
    },

    -- Shows keybindings in popup
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        config = true
    },

    -- Highlight, list and search todo comments in your projects
    {
        "folke/todo-comments.nvim",
        lazy = false,
        depencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require("plugins.todo-comments").config() end
    },

    -- The fastest colorizer, #a6e22e
    {
        "NvChad/nvim-colorizer.lua",
        lazy = false,
        config = function() require("plugins.nvim-colorizer").config() end
    },

    -- Indent guides for Neovim
    {
        "lukas-reineke/indent-blankline.nvim",
        lazy = false,
        config = function() require("plugins.indent-blankline").config() end
    },

    -- Rename tabs
    {
        "gcmt/taboo.vim"
    }
}
