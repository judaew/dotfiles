return {
    {
        "hrsh7th/nvim-cmp",
        event = "InsertEnter",
        keys = { ":", "/", "?" },
        dependencies = {
            -- autopairing of (){}[] etc
            {
                "windwp/nvim-autopairs",
                config = function()
                    require("nvim-autopairs").setup({
                        fast_wrap = {},
                        disable_filetype = { "TelescopePrompt", "vim" },
                    })

                    -- setup cmp for autopairs
                    local cmp_autopairs = require("nvim-autopairs.completion.cmp")
                    require("cmp").event:on("confirm_done", cmp_autopairs.on_confirm_done())
                end,
            },
            -- cmp sources plugins
            {
                "hrsh7th/cmp-nvim-lsp",
                "hrsh7th/cmp-buffer",
                "hrsh7th/cmp-cmdline",
                "hrsh7th/cmp-nvim-lua",
                "andersevenrud/cmp-tmux"
            },
            -- snippet plugin
            {
                "hrsh7th/vim-vsnip",
                dependencies = {
                    "hrsh7th/cmp-vsnip"
                },
                config = function() require("plugins.snippets").config() end
            },
            -- git
            { "petertriho/cmp-git" }
            -- TODO: "paopaol/cmp-doxygen"
        },
        config = function() require("plugins.ide.intellisense").config() end
    }
}
