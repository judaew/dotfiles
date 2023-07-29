local map = vim.keymap.set

return {
    -- Dynamically interact with registers
    {
        "gennaro-tedesco/nvim-peekup",
        lazy = false,
        keys = {
            { '""',  desc = "Open peekup window" },
            { '"x',  desc = "Empty all registers" },
        }
    },

    -- Last edit position
    {
        "ethanholz/nvim-lastplace",
        lazy = false,
        config = function() require("plugins.nvim-lastplace").config() end
    },

    -- Land on window you chose like tmux's 'display-pane'
    {
        "t9md/vim-choosewin",
        event = "VeryLazy",
        config = function()
            map("n", "-", "<Plug>(choosewin)",
                { desc = "Choosewin", noremap=true })
        end
    },

    -- Better quickfix window
    {
        "kevinhwang91/nvim-bqf",
        ft = "qf",
        config = function()
            require("bqf").setup({
                preview = {
                    winblend = 0
                }
            })
        end
    },

    -- Viewer & Finder for LSP symbols and tags
    {
        -- TODO:
        -- * Show tag in the statusline, see:
        --   https://github.com/liuchengxu/vista.vim#show-the-nearest-methodfunction-in-the-statusline
        "liuchengxu/vista.vim",
        lazy = false,
        config = function() require("plugins.tagbar").keys() end
    },

    -- UndoTree
    {
        "simnalamburt/vim-mundo",
        -- Plugin can't be lazy loaded
        lazy = false,
        config = function()
            map("n", "<Leader>u", ":MundoToggle<CR>",
                { desc="Toggle UndoTree (via Mundo)", noremap=true })
        end
    },
}
