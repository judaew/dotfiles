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
                config = function()
                    vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/.local/share/nvim/vsnip"

                    vim.cmd([[
                    " Expand
                    imap <expr> <C-j>   vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<C-j>'
                    smap <expr> <C-j>   vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<C-j>'

                    " Expand or jump
                    imap <expr> <C-l>   vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
                    smap <expr> <C-l>   vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

                    " Jump forward or backward
                    imap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
                    smap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
                    imap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
                    smap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
                    ]])
                end
            },
            -- git
            { "petertriho/cmp-git" }
            -- TODO: "paopaol/cmp-doxygen"
        },
        config = function() require("plugins.ide.intellisense").config() end
    }
}
