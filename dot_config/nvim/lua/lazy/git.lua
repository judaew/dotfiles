local map = function(keys, func, desc)
  vim.keymap.set("n", keys, func, { desc = desc, noremap = true })
end

return {
  -- Git wrapper
  {
    "tpope/vim-fugitive",
    lazy = false,
    init = function() require("utils.lazy").git_load("gv.vim") end,
    dependencies = {
      "tpope/vim-rhubarb"       -- For enable :Gbrowse
    }
  },

  -- Another git wrapper
  {
    "NeogitOrg/neogit",
    lazy = false,
    dependencies = "nvim-lua/plenary.nvim",
    config = true
  },

  -- Git commit browser
  {
    "junegunn/gv.vim",
    lazy = true,
    keys = {
      { "<F4>",   desc = "Commit browser" },
      { "<S-F4>", desc = "Commit browser" }
    },
    init = function() require("utils.lazy").git_load("gv.vim") end,
    config = function()
      -- Function keys
      map("<F4>", "<cmd>GV<CR>", "Commit browser")
      map("<S-F4>", "<cmd>GV!<CR>", "Commit browser")
    end
  },

  -- Git signs
  {
    "lewis6991/gitsigns.nvim",
    lazy = true,
    init = function() require("utils.lazy").git_load("gitsigns.nvim") end,
    dependencies = {
      "nvim-lua/plenary.nvim"
    },
    config = function()
      require("gitsigns").setup({
        -- make gitsigns look like vim-signify
        signs = {
          add          = { text = "+" },
          change       = { text = "!" },
          delete       = { text = "_", show_count = true },
          topdelete    = { text = "‾", show_count = true },
          changedelete = { text = "~", show_count = true },
        }
      })
      -- Highlight signs, see:
      -- https://github.com/lewis6991/gitsigns.nvim/commit/3d7e49c201537ee0293a1a3abe67b67f8e7648a5
      vim.api.nvim_set_hl(0, 'GitSignsAdd', { link = 'DiffAdd' })
      vim.api.nvim_set_hl(0, 'GitSignsChange', { link = 'DiffChange' })
      vim.api.nvim_set_hl(0, 'GitSignsChangedelete', { link = 'DiffChange' })
      vim.api.nvim_set_hl(0, 'GitSignsDelete', { link = 'DiffDelete' })
      vim.api.nvim_set_hl(0, 'GitSignsTopdelete', { link = 'DiffDelete' })
    end
  },
}
