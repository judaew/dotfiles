local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, expr=true, noremap=true})
end

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

    -- Annotation generator
    {
        "danymat/neogen",
        keys = function()
            map("<Leader>cc", function() require("neogen").generate({}) end,
                "Neogen Comment")
        end,
        dependencies = { "hrsh7th/vim-vsnip" },
        config = function()
            require("neogen").setup({
                snippet_engine = "vsnip",
                languages = {
                    lua = {
                        template = {
                            annotation_convention = "ldoc"
                        }
                    }
                }
            })
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

    -- Refactoring
    {
        "ThePrimeagen/refactoring.nvim",
        dependencies = {
            { "nvim-lua/plenary.nvim" },
            { "nvim-treesitter/nvim-treesitter" }
        },
        keys = {
            {
                "<Leader>r", function() require("refactoring").select_refactor() end,
                mode="v", noremap=true, desc="Refactoring"
            }
        },
        opts = {}
    },

    -- Better increase/descrease
    {
        "monaqa/dial.nvim",
        keys = {
            { "<C-a>", desc = "Increment" },
            { "<C-x>", desc = "Decrement" },
        },
        config = function()
            local augend = require("dial.augend")
            require("dial.config").augends:register_group({
            default = {
                augend.integer.alias.decimal, -- 0, 1, 2, 3, ...
                augend.integer.alias.hex,     -- 0x01, 0x1a1f, ...
                augend.date.alias["%Y/%m/%d"],-- 2023/07/31
                augend.date.alias["%Y-%m-%d"],-- 2023-07-31
                augend.constant.alias.bool,   -- true <-> false
                augend.semver.alias.semver,   -- 0.3.0, 1.22.1, 3.9.1, ...
                augend.constant.new({elements={"let", "const"}}), -- let <-> const
                },
            })

            map("<C-a>", function() return require("dial.map").inc_normal() end, "Increment")
            map("<C-x>", function() return require("dial.map").dec_normal() end, "Decrement")
        end
    },

    -- Paste image from clipboard
    -- {
    --     "TobinPalmer/pastify.nvim",
    --     cmd = { "Pastify" },
    --     config = function()
    --         require("pastify").setup({})
    --     end
    -- }
    -- {
    --     "TobinPalmer/pastify.nvim"
    -- }
}
