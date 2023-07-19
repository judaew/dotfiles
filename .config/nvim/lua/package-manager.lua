local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local keymap = vim.keymap.set
require "lazy".setup({
    -- *** IDE section (LSP, DAP, linting, snippets)
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            -- plugin for automatically configures lua-language-server for
            -- Neovim config, Neovim runtime and plugin directories
            "folke/neodev.nvim",
            -- plugin for better action menu
            "weilbith/nvim-code-action-menu",
            -- VSCode 💡 for neovim's built-in LSP
            "kosayoda/nvim-lightbulb",
            -- Clangd's off-spec features
            "p00f/clangd_extensions.nvim"
        },
        config = function() require "plugins/ide/lsp" end
    },
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            -- plugin for UI
            "rcarriga/nvim-dap-ui"
        },
        config = function() require "plugins/ide/dap" end
    },
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function() require "plugins/ide/treesitter" end
    },
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        after = "nvim-treesitter",
        requires = "nvim-treesitter/nvim-treesitter"
    },
    {
        "ms-jpq/coq_nvim",
        branch = "coq",
        build = ":COQdeps",
        init = function() require "plugins/ide/intellisense" end,
    },
    {
        "Shatur/neovim-session-manager",
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require "plugins/session_manager" end
    },
    {
        "stevearc/overseer.nvim",
        dependencies = {
            "stevearc/dressing.nvim"
        },
        config = function() require "plugins/task_runner" end
    },
    {
        "michaelb/sniprun",
        config = function() require "plugins/code_runner" end
    },

    -- *** Specific Language Support / Syntax Highlighting / Formatting
    { "gpanders/editorconfig.nvim" },
    {
        "fatih/vim-nginx",
        enabled = false
    },
    {
        "MTDL9/vim-log-highlighting",
        ft = { "log" }
    },
    {
        dir = "~/Workspaces/github.com/judaew/macports.nvim",
        ft = "Portfile"
    },

    -- *** Special Features
    { "tpope/vim-eunuch" },

    -- *** Movement
    {
        "ethanholz/nvim-lastplace",
        config = function () require "plugins/nvim-lastplace" end
    },
    {
        -- TODO:
        -- * Show tag in the statusline, see:
        --   https://github.com/liuchengxu/vista.vim#show-the-nearest-methodfunction-in-the-statusline
        "liuchengxu/vista.vim",
        config = function() require "plugins/tagbar" end
    },
    {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" }
    },
    {
        "junegunn/fzf.vim",
        dependencies = { "junegunn/fzf" }
    },
    {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function() return vim.fn.executable "make" == 1 end,
        config = function() require "plugins/telescope" end
    },
    {
        "ms-jpq/chadtree",
        branch = "chad",
        build = ":CHADdeps",
        config = function() require "plugins/file_manager" end
    },
    {
        "justinmk/vim-gtfo",
        config = function() vim.cmd([[
            let g:gtfo#terminals = { "mac": "kitty" }
        ]]) end
    },
    {
        "simnalamburt/vim-mundo",
        config = function()
            keymap("n", "<Leader>u", ":MundoToggle<CR>", { desc = "MundoToggle", noremap = true, silent = true })
        end
    },
    {
        "t9md/vim-choosewin",
        config = function()
            keymap("n", "-", "<Plug>(choosewin)", { desc = "Choosewin", noremap = true, silent = true })
        end
    },

    -- *** Text Manipulation
    {
        "numToStr/Comment.nvim",
        opts = { mappings = { basic = true, extra = false } }
    },
    {
        "lyokha/vim-xkbswitch",
        init = function()
            vim.g.XkbSwitchEnabled = 1
            vim.g.XkbSwitchLib = "/opt/local/lib/libInputSourceSwitcher.dylib"
        end
    },
    { -- Detect and adjusts tabstop and shiftwidth automatically
        "tpope/vim-sleuth",
    },

    -- *** Git
    { "tpope/vim-fugitive" },
    { "tpope/vim-rhubarb" }, -- For enable :Gbrowse
    { "junegunn/gv.vim", config = function() require "plugins/gv" end },
    {
        "lewis6991/gitsigns.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require "plugins/gitsigns" end
    },
    {
        "NeogitOrg/neogit",
        dependencies = "nvim-lua/plenary.nvim",
        config = true
    },

    -- *** UI
    {
        "goolord/alpha-nvim",
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        event = "VimEnter",
        config = function () require "plugins/alpha-nvim" end
    },
    {
        "famiu/feline.nvim",
        config = function() require "plugins/feline" end
    },
    { "stevearc/dressing.nvim", opts = {} },
    {
        "folke/which-key.nvim",
        config = true
    },
    { "junegunn/vim-peekaboo" },
    {
        "folke/todo-comments.nvim",
        depencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require "plugins/todo-comments" end
    },
    {
        "norcalli/nvim-colorizer.lua",
        config = function() require "plugins/nvim-colorizer" end
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        opts = {
            show_trailing_blankline_indent = false,
            show_current_context = true,
            use_treesitter = true
        }
    },

    -- *** Themes
    {
        "fatih/molokai",
        enabled = true,
        priority = 1000,
        config = function() vim.cmd ([[
            set background=dark
            colorscheme molokai
            let g:molokai_origin=1
            let g:rehash=1
        ]]) end
    },
}, {
    ui = {
        icons = {
            cmd = "⌘",
            config = " ",
            event = " ",
            ft = " ",
            init = " ",
            keys = " ",
            plugin = " ",
            runtime = " ",
            source = " ",
            start = " ",
            task = " ",
            lazy = " ",
        },
    },
})
