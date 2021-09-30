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
        use {'hrsh7th/nvim-cmp', config=req 'lsp/cmp',
                requires =
                {
                    'hrsh7th/cmp-nvim-lsp',
                    'hrsh7th/cmp-buffer',
                }
        }
        use {'neovim/nvim-lspconfig', config=req 'lsp/lspconfig'}
        use {'hrsh7th/vim-vsnip', config=runtime 'vim-vsnip',
                requires =
                {
                    'hrsh7th/nvim-cmp',
                    'hrsh7th/cmp-vsnip',
                    'rafamadriz/friendly-snippets',
                }
        }

        -- ### Specific Language Support / Syntax Highlighting / Formatting
        use 'editorconfig/editorconfig-vim'
        use 'fatih/vim-nginx'
        use 'ekalinin/dockerfile.vim'
        use 'dzeban/vim-log-syntax'
        use '~/Projects/judaew/vim-macports'
        use {'habamax/vim-asciidoctor', config=runtime 'vim-asciidoctor'}

        -- ### Special Features
        use 'tpope/vim-dispatch'

        -- ### Movement
        use 'junegunn/fzf.vim'
        use {'junegunn/fzf.vim', config=runtime 'fzf'}
        use {'lambdalisue/fern.vim', config=req 'fern'}
        use {'justinmk/vim-gtfo', config=runtime 'vim-gtfo'}
        use {'ratfactor/vviki', config=req 'vviki'}
        use {'simnalamburt/vim-mundo', config=req 'vim-mundo'}
        use {'t9md/vim-choosewin', config=req 'vim-choosewin'}

        -- ### Text Manipulation
        use 'tpope/vim-repeat'
        use 'tpope/vim-endwise'
        use {'b3nj5m1n/kommentary', config=req 'kommentary'}
        use {'lyokha/vim-xkbswitch', config=runtime 'vim-xkbswitch'}

        -- Git
        use 'tpope/vim-fugitive'
        use 'junegunn/gv.vim'
        use {'lewis6991/gitsigns.nvim', config=req 'gitsigns',
                requires = {'nvim-lua/plenary.nvim'}
        }

        -- ### UI
        use {'famiu/feline.nvim', config=req 'feline'}
        use 'machakann/vim-highlightedyank'
        use 'junegunn/vim-peekaboo'

        -- ### Themes
        use 'fatih/molokai'
    end
}
