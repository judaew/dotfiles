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
        "m-demare/hlargs.nvim",
        enabled = false,
        requires = "nvim-treesitter/nvim-treesitter",
        config = function()
            require('hlargs').setup()
        end
    },
    {
        "ms-jpq/coq_nvim",
        branch = "coq",
        init = function() require "plugins/ide/intellisense" end,
    },
    {
        "Shatur/neovim-session-manager",
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require "plugins/neovim-session-manager" end
    },
    {
        "Dax89/automaton.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope.nvim",
            -- Debug support for "launch" configurations (Optional)
            "mfussenegger/nvim-dap"
        },
        config = function() require "plugins/ide/automaton" end
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
        ft = "Portfile",
        config = function()
            -- Load snippets
            vim.g.macports_snippets = 1
            -- Load completefunc
            vim.g.macports_completefunc = 1
        end
    },

    -- *** Special Features
    { "tpope/vim-dispatch" },

    -- *** Movement
    {
        "ethanholz/nvim-lastplace",
        opts = {
            lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
            lastplace_ignore_filetype = { "gitcommit", "gitrebase" },
            lastplace_open_folds = true
        }
    },
    -- TODO:
    -- * Show tag in the statusline, see:
    --   https://github.com/liuchengxu/vista.vim#show-the-nearest-methodfunction-in-the-statusline
    -- * Add hotkey, e. g. F8 key
    { "liuchengxu/vista.vim" },
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
        "lambdalisue/fern.vim",
        config = function()
            keymap("n", "<Leader>n", ":Fern . -drawer<CR>", { desc = "fern (pwd)", noremap = true, silent = true })
            keymap("n", "<Leader>f", ":Fern . -reveal=% -drawer<CR>", { desc = "fern (current dir)", noremap = true, silent = true })
        end
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
    { "tpope/vim-eunuch" },
    { "mg979/vim-visual-multi" },

    -- *** Text Manipulation
    { "tpope/vim-repeat" },
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
    { -- Detect tabstop and shiftwidth automatically
        "tpope/vim-sleuth",
        enabled = false
    },

    -- *** Git
    { "tpope/vim-fugitive" },
    { "tpope/vim-rhubarb" }, -- For enable :Gbrowse
    { "junegunn/gv.vim" },
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
    { -- Theme inspired by Atom
        "navarasu/onedark.nvim",
        enabled = false,
        priority = 1000,
        config = function()
          vim.cmd.colorscheme "onedark"
        end,
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
