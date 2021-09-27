---------------------------------------
-- GIT . . . . . . . . . . . . . . . --
---------------------------------------

-- setup lewis6991/gitsigns.nvim
require('gitsigns').setup {
  -- make gitsigns look like vim-signify
  signs = {
    add          = {hl = 'DiffAdd'   , text = '+'},
    change       = {hl = 'DiffChange', text = '!'},
    delete       = {hl = 'DiffDelete', text = '_', show_count=true},
    topdelete    = {hl = 'DiffDelete', text = '‾', show_count=true},
    changedelete = {hl = 'DiffChange', text = '~', show_count=true},
  }
}

---------------------------------------
-- DEV & LSP . . . . . . . . . . . . --
---------------------------------------

-- setup hrsh7th/nvim-cmp.
local cmp = require'cmp'

cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` user.
    end,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
  },
  sources = {
    { name = 'nvim_lsp' },
    -- For vsnip user.
    { name = 'vsnip' },
    { name = 'buffer' },
  }
})

-- setup neovim/lspconfig
require'lspconfig'.clangd.setup{
  capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities());
  cmd = { "clangd-mp-12", "--background-index" };
}
