-- Install and load Packer
local install_path = vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if not vim.loop.fs_stat(vim.fn.glob(install_path)) then
    os.execute('git clone https://github.com/wbthomason/packer.nvim '..install_path)
end

-- Source a Lua file
local function req(plugin)
    return 'require "plugin/'..plugin..'"'
end

-- Source a Vim file
local function runtime(plugin)
    return 'vim.api.nvim_command "runtime viml/plugin/'..plugin..'.vim"'
end

-- Load plugins
vim.cmd 'packadd packer.nvim'

local packer = require 'packer'

packer.init {compile_path = vim.fn.stdpath('config')..'/lua/init/packer_compiled.lua'}

return packer.startup
{
    function(use)
    use {'wbthomason/packer.nvim', opt=true}

        -- ### Language Servers / Linting / Snippets
        use {'neovim/nvim-lspconfig', config=req 'lsp/lspconfig'}
        use {'nvim-treesitter/nvim-treesitter',
                run = ':TSUpdate', config=req 'nvim-treesitter'}
        use {'hrsh7th/nvim-cmp', config=req 'lsp/cmp',
                requires = {
                    'hrsh7th/cmp-nvim-lsp',
                    'hrsh7th/cmp-buffer'
                }
        }
        use {'hrsh7th/vim-vsnip', config=runtime 'vim-vsnip',
                requires = {
                    'hrsh7th/nvim-cmp',
                    'hrsh7th/cmp-vsnip'
                    -- 'rafamadriz/friendly-snippets',
                }
        }

        -- ### Specific Language Support / Syntax Highlighting / Formatting
        use 'editorconfig/editorconfig-vim'
        use 'fatih/vim-nginx'
        use 'MTDL9/vim-log-highlighting'
        use {'~/Projects/judaew/macports.nvim', config=req 'macports'}
        use {'habamax/vim-asciidoctor', config=runtime 'vim-asciidoctor'}

        -- ### Special Features
        use 'tpope/vim-dispatch'

        -- ### Movement
        use 'farmergreg/vim-lastplace'
        use {'junegunn/fzf.vim', config=runtime 'fzf'}
        use {'lambdalisue/fern.vim', config=runtime 'fern'}
        use {'justinmk/vim-gtfo', config=runtime 'vim-gtfo'}
        use {'simnalamburt/vim-mundo', config=runtime 'vim-mundo'}
        use {'t9md/vim-choosewin', config=runtime 'vim-choosewin'}
        use 'tpope/vim-eunuch'

        -- ### Text Manipulation
        use 'tpope/vim-repeat'
        use 'tpope/vim-endwise'
        use {'b3nj5m1n/kommentary', config=req 'kommentary'}
        use {'lyokha/vim-xkbswitch', config=runtime 'vim-xkbswitch'}

        -- ### Git
        use 'tpope/vim-fugitive'
        use 'junegunn/gv.vim'
        use {'lewis6991/gitsigns.nvim', config=req 'gitsigns',
                requires = {'nvim-lua/plenary.nvim'}
        }

        -- ### UI
        use {'famiu/feline.nvim', config=req 'feline'}
        use 'machakann/vim-highlightedyank'
        use 'junegunn/vim-peekaboo'
        use {'folke/todo-comments.nvim', config=req 'todo-comments',
                requires = {'nvim-lua/plenary.nvim'}}

        -- ### Themes
        use 'fatih/molokai'

        -- ### Load local plugins
        if vim.fn.has('macunix') then
            use {'/opt/local/share/vim/vimfiles'}
        end
    end
}
