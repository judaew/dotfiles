local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

return {
    -- Git wrapper
    {
        "tpope/vim-fugitive",
        lazy = false,
        init = function() require("utils.lazy").git_load("gv.vim") end,
        dependencies = {
            "tpope/vim-rhubarb" -- For enable :Gbrowse
        }
    },

    -- Another git wrapper
    {
        "NeogitOrg/neogit",
        lazy = false,
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
        config = function()
            -- Function keys
            map("<F4>",   "<cmd>GV<CR>",  "Commit browser")
            map("<S-F4>", "<cmd>GV!<CR>", "Commit browser")
        end
    },

    -- Git signs
    {
        "lewis6991/gitsigns.nvim",
        lazy = true,
        init = function() require("utils.lazy").git_load("gitsigns.nvim") end,
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function()
            require("gitsigns").setup({
                -- make gitsigns look like vim-signify
                signs = {
                    add          = {hl = "DiffAdd"   , text = "+"},
                    change       = {hl = "DiffChange", text = "!"},
                    delete       = {hl = "DiffDelete", text = "_", show_count=true},
                    topdelete    = {hl = "DiffDelete", text = "‾", show_count=true},
                    changedelete = {hl = "DiffChange", text = "~", show_count=true},
                },
            })
        end
    },
}
