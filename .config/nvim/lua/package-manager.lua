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
require 'lazy'.setup({
    -- *** IDE section (LSP, DAP, linting, snippets)
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            'hrsh7th/nvim-cmp',
            'folke/neodev.nvim',
            'weilbith/nvim-code-action-menu'
        },
        config=function() require 'plugins/ide/lsp' end
    },
    {
        'mfussenegger/nvim-dap',
        dependencies = {
            -- plugin for UI
            'rcarriga/nvim-dap-ui'
        },
        config=function() require 'plugins/ide/dap' end
    },
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            -- plugin for additional functions
            'nvim-treesitter/nvim-treesitter-textobjects'
        },
        build = ':TSUpdate',
        config=function() require 'plugins/ide/treesitter' end
    },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-buffer'
        },
        config=function() require 'plugins/ide/intellisense' end
    },
    {
        'hrsh7th/vim-vsnip',
        dependencies = {
            'hrsh7th/nvim-cmp',
            'hrsh7th/cmp-vsnip'
        },
        config=function() require 'plugins/vim-vsnip' end
    },

    -- *** Specific Language Support / Syntax Highlighting / Formatting
    { 'gpanders/editorconfig.nvim' },
    { 'fatih/vim-nginx' },
    { 'MTDL9/vim-log-highlighting' },
    {
        dir = '~/Workspaces/github.com/judaew/macports.nvim',
        config=function()
            -- Load snippets
            vim.g.macports_snippets = 1
            -- Load completefunc
            vim.g.macports_completefunc = 1
        end
    },

    -- *** Special Features
    { 'tpope/vim-dispatch' },
    {
        'lewis6991/impatient.nvim',
        config=function() require('impatient') end
    },

    -- *** Movement
    -- Error detected while processing function <SNR>59_lastplace
    -- { 'farmergreg/vim-lastplace' }
    { 'preservim/tagbar' },
    {
        'nvim-telescope/telescope.nvim',
        version = '*',
        dependencies = { 'nvim-lua/plenary.nvim' }
    },
    {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function() return vim.fn.executable 'make' == 1 end,
        config = function() require 'plugins/telescope' end
    },
    {
        'lambdalisue/fern.vim',
        config=function()
            keymap('n', '<Leader>n', ':Fern . -drawer<CR>', { desc = "fern (pwd)", noremap = true, silent = true })
            keymap('n', '<Leader>f', ':Fern . -reveal=% -drawer<CR>', { desc = "fern (current dir)", noremap = true, silent = true })
        end
    },
    {
        'justinmk/vim-gtfo',
        config=function() vim.cmd([[
            let g:gtfo#terminals = { 'mac': 'kitty' }
        ]]) end
    },
    {
        'simnalamburt/vim-mundo',
        config=function()
            keymap('n', '<Leader>u', ':MundoToggle<CR>', { desc = "MundoToggle", noremap = true, silent = true })
        end
    },
    {
        't9md/vim-choosewin',
        config=function()
            keymap('n', '-', '<Plug>(choosewin)', { desc = "Choosewin", noremap = true, silent = true })
        end
    },
    { 'tpope/vim-eunuch' },

    -- *** Text Manipulation
    { 'tpope/vim-repeat' },
    {
        'numToStr/Comment.nvim',
        opts = { mappings = { basic = true, extra = false } }
    },
    {
        'lyokha/vim-xkbswitch',
        init=function()
            vim.g.XkbSwitchEnabled = 1
            vim.g.XkbSwitchLib = '/opt/local/lib/libInputSourceSwitcher.dylib'
        end
    },
    { -- Detect tabstop and shiftwidth automatically
        'tpope/vim-sleuth'
    },

    -- *** Git
    { 'tpope/vim-fugitive' },
    { 'tpope/vim-rhubarb' }, -- For enable :Gbrowse
    { 'junegunn/gv.vim' },
    {
        'lewis6991/gitsigns.nvim',
        dependencies = {
            'nvim-lua/plenary.nvim'
        },
        config=function() require 'plugins/gitsigns' end
    },
    {
        'TimUntersberger/neogit',
        dependencies = 'nvim-lua/plenary.nvim',
        config=true
    },

    -- *** UI
    {
        'famiu/feline.nvim',
        config=function() require 'plugins/feline' end
    },
    {
        'folke/which-key.nvim',
        config=true
    },
    { 'junegunn/vim-peekaboo' },
    {
        'folke/todo-comments.nvim',
        depencies = {
            'nvim-lua/plenary.nvim'
        },
        config=function() require 'plugins/todo-comments' end
    },
    {
        'norcalli/nvim-colorizer.lua',
        config=function() require 'plugins/nvim-colorizer' end
    },
    {
        'lukas-reineke/indent-blankline.nvim',
        opts = {
            -- char = '┊',
            show_trailing_blankline_indent = false,
            show_current_context = true
        }
    },

    -- *** Themes
    {
        'fatih/molokai',
        enabled=true,
        priority = 1000,
        config=function() vim.cmd ([[
            set background=dark
            colorscheme molokai
            let g:molokai_origin=1
            let g:rehash=1
        ]]) end
    },
    { -- Theme inspired by Atom
        'navarasu/onedark.nvim',
        enabled=false,
        priority = 1000,
        config = function()
          vim.cmd.colorscheme 'onedark'
        end,
    },
})
