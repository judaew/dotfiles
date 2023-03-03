local fn = vim.fn

-- Automatically install Packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
local packer_bootstrap = {}
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system {
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    }
    print "Installing packer close and reopen Neovim..."
    vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
    augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

packer.init {
    display = {
        open_fn = function()
            return require("packer.util").float { border = "rounded" }
        end,
    },
    compile_path = vim.fn.stdpath('config')..'/lua/init/packer_compiled.lua'
}

-- Source a Lua file
local function req(plugin)
    return 'require "plugin/'..plugin..'"'
end

-- Source a Vim file
local function runtime(plugin)
    return 'vim.api.nvim_command "runtime viml/plugin/'..plugin..'.vim"'
end

return packer.startup(function(use)
    use "wbthomason/packer.nvim"

    -- ### LSP / Linting / DAP / Snippets
    use {'neovim/nvim-lspconfig', config=req 'lsp/lspconfig'}
    use {'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate', config=req 'nvim-treesitter',
        -- plugin for additional functions
        requires = {'nvim-treesitter/nvim-treesitter-textobjects'}
    }
    use {'hrsh7th/nvim-cmp', config=req 'lsp/cmp',
            requires = {
                'hrsh7th/cmp-nvim-lsp',
                'hrsh7th/cmp-buffer'
        }
    }
    use {'mfussenegger/nvim-dap', config=req 'lsp/dap',
        -- plugin for UI
        requires = {'rcarriga/nvim-dap-ui'}
    }
    use {'hrsh7th/vim-vsnip', config=runtime 'vim-vsnip',
            requires = {
                'hrsh7th/nvim-cmp',
                'hrsh7th/cmp-vsnip'
        }
    }

    -- ### Specific Language Support / Syntax Highlighting / Formatting
    use 'gpanders/editorconfig.nvim'
    use 'fatih/vim-nginx'
    use 'MTDL9/vim-log-highlighting'
    use {'~/Workspaces/github.com/judaew/macports.nvim', config=req 'macports'}
    use {'habamax/vim-asciidoctor', config=runtime 'vim-asciidoctor'}

    -- ### Special Features
    use 'tpope/vim-dispatch'
    use {'lewis6991/impatient.nvim', config=req 'impatient'}

    -- ### Movement
    -- Error detected while processing function <SNR>59_lastplace
    -- use 'farmergreg/vim-lastplace'
    use 'preservim/tagbar'
    use {'junegunn/fzf.vim', config=runtime 'fzf'}
    use {'lambdalisue/fern.vim', config=runtime 'fern'}
    use {'justinmk/vim-gtfo', config=runtime 'vim-gtfo'}
    use {'simnalamburt/vim-mundo', config=runtime 'vim-mundo'}
    use {'t9md/vim-choosewin', config=runtime 'vim-choosewin'}
    use 'tpope/vim-eunuch'

    -- ### Text Manipulation
    use 'tpope/vim-repeat'
    -- use 'tpope/vim-endwise'
    use {'b3nj5m1n/kommentary', config=req 'kommentary'}
    use {'lyokha/vim-xkbswitch', config=runtime 'vim-xkbswitch'}

    -- ### Git
    use 'tpope/vim-fugitive'
    use 'tpope/vim-rhubarb' -- For enable :Gbrowse
    use 'junegunn/gv.vim'
    use {'lewis6991/gitsigns.nvim', config=req 'gitsigns',
            requires = {'nvim-lua/plenary.nvim'}
    }
    use { 'TimUntersberger/neogit', config=req 'neogit',
        requires = 'nvim-lua/plenary.nvim'
    }

    -- ### UI
    use {'famiu/feline.nvim', config=req 'feline'}
    use 'junegunn/vim-peekaboo'
    use {'folke/todo-comments.nvim', config=req 'todo-comments',
            requires = {'nvim-lua/plenary.nvim'}}
    use {'norcalli/nvim-colorizer.lua', config=req 'nvim-colorizer'}
    use {'lewis6991/hover.nvim', config=req 'hover'}

    -- ### Themes
    use 'fatih/molokai'

    -- ### Load local plugins
    if vim.fn.has('macunix') then
        use {'/opt/local/share/vim/vimfiles'}
    end

    -- Automatically set up your configuration after cloning packer.nvim
    if not packer_bootstrap then
        require("packer").sync()
    end
end)
