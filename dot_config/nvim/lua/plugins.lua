-- This file can be loaded by calling `lua require('plugins')` from init.vim

return require('packer').startup(function()
    -- package management
    use 'wbthomason/packer.nvim'

    -- theme
    use 'fatih/molokai'

    -- ui
    use 'itchyny/lightline.vim'
    use 'machakann/vim-highlightedyank'
    use 'junegunn/vim-peekaboo'
    use 't9md/vim-choosewin'

    -- edit
    use 'tpope/vim-repeat'
    use 'tpope/vim-endwise'
    use 'tpope/vim-commentary'
    use 'tpope/vim-dispatch'
    use {'simnalamburt/vim-mundo', cmd = {'MundoToggle'}}
    use 'lyokha/vim-xkbswitch'

    -- browsing and search
    use {'lambdalisue/fern.vim', cmd = {'Fern'}}
    use 'justinmk/vim-gtfo'
    use 'junegunn/fzf.vim'
    use 'ratfactor/vviki'

    -- git
    use 'tpope/vim-fugitive'
    use 'junegunn/gv.vim'
    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'}
    }

    -- lsp
    use 'neovim/nvim-lspconfig'
    use {
        'hrsh7th/nvim-cmp',
        requires = {{'hrsh7th/cmp-nvim-lsp'},
                    {'hrsh7th/cmp-buffer'}}
    }

    -- snippets
    use {
        'hrsh7th/vim-vsnip',
        requires = {{'hrsh7th/nvim-cmp'},
                    {'hrsh7th/cmp-vsnip'}}

    }
    -- use 'rafamadriz/friendly-snippets'
    
    -- lang
    use 'editorconfig/editorconfig-vim'
    use 'fatih/vim-nginx'
    use 'ekalinin/dockerfile.vim'
    use 'dzeban/vim-log-syntax'
    use '~/Projects/judaew/vim-macports'
    use 'habamax/vim-asciidoctor'
end)
