local map = vim.keymap.set

return {
  -- Last edit position
  {
    "ethanholz/nvim-lastplace",
    lazy = false,
    config = function()
      require("nvim-lastplace").setup({
        lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
        lastplace_ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit" },
        lastplace_open_folds = true
      })
    end
  },

  -- Land on window you chose like tmux's 'display-pane'
  {
    "t9md/vim-choosewin",
    event = "VeryLazy",
    config = function()
      map("n", "-", "<Plug>(choosewin)",
        { desc = "Choosewin", noremap = true })
    end
  },

  -- Pretty diagnostics, quickfix, location and LSP lists
  -- TODO: conf
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    config = function()
      map("n", "<Leader>q", function() require("trouble").open("document_diagnostics") end)
      map("n", "<Leader>Q", function() require("trouble").open("workspace_diagnostics") end)
    end
  },

  -- Viewer & Finder for LSP symbols
  {
    "simrat39/symbols-outline.nvim",
    keys = {
      { "<F9>", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" }
    },
    cmd = "SymbolsOutline",
    opts = {
      relative_width = false,
      width = 40,
      symbols = {
        File          = { icon = "󰈙 ", hl = "@text.uri" },
        Module        = { icon = " ", hl = "@namespace" },
        Namespace     = { icon = "󰅪 ", hl = "@namespace" },
        Package       = { icon = "󰏗 ", hl = "@namespace" },
        Class         = { icon = "󰠱 ", hl = "@type" },
        Method        = { icon = "󰆧 ", hl = "@method" },
        Property      = { icon = "󰜢 ", hl = "@method" },
        Field         = { icon = "󰇽 ", hl = "@field" },
        Constructor   = { icon = " ", hl = "@constructor" },
        Enum          = { icon = " ", hl = "@type" },
        Interface     = { icon = " ", hl = "@type" },
        Function      = { icon = "󰊕 ", hl = "@function" },
        Variable      = { icon = "󰂡 ", hl = "@constant" },
        Constant      = { icon = "󰏿 ", hl = "@constant" },
        String        = { icon = "󱌯 ", hl = "@string" },
        Number        = { icon = "󰎠 ", hl = "@number" },
        Boolean       = { icon = "󰦍 ", hl = "@boolean" },
        Array         = { icon = "󱒅 ", hl = "@constant" },
        Object        = { icon = "󰀚 ", hl = "@type" },
        Key           = { icon = "󰌋 ", hl = "@type" },
        Null          = { icon = "󰟢 ", hl = "@type" },
        EnumMember    = { icon = " ", hl = "@field" },
        Struct        = { icon = " ", hl = "@type" },
        Event         = { icon = " ", hl = "@type" },
        Operator      = { icon = "󰆕 ", hl = "@operator" },
        TypeParameter = { icon = "󰅲 ", hl = "@parameter" },
        Component     = { icon = " ", hl = "@function" },
        Fragment      = { icon = " ", hl = "@constant" },
      }
    }
  },

  -- UndoTree
  {
    "simnalamburt/vim-mundo",
    -- Plugin can't be lazy loaded
    lazy = false,
    config = function()
      map("n", "<Leader>u", ":MundoToggle<CR>",
        { desc = "Toggle UndoTree (via Mundo)", noremap = true })
    end
  },
}
