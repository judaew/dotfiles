return {
  -- Improve the default vim.ui interfaces
  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    opts = {}
  },

  -- Shows keybindings in popup
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    -- config = true
    opts = {
      icons = {
        breadcrumb = ">>",
        separator = "->",
        group = "+"
      },
    }
  },

  -- Highlight, list and search todo comments in your projects
  {
    "folke/todo-comments.nvim",
    lazy = false,
    dependencies = {
      "nvim-lua/plenary.nvim"
    },
    config = function()
      require("todo-comments").setup({
        signs = true,
        sign_priority = 8,
        keywords = {
          TODO = { icon = " ", color = "info" },
          NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
          FIX  = {
            icon = " ",
            color = "error",
            alt =
            { "FIXME", "BUG", "FIXIT", "ISSUE" }
          },
          HACK = { icon = " ", color = "warning" },
          WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
          PERF = {
            icon = " ",
            color = "perf",
            alt =
            { "OPTIM", "PERFORMANCE", "OPTIMIZE" }
          },
          TEST = {
            icon = " ",
            color = "test",
            alt =
            { "TESTING", "PASSED", "FAILED", "DEBUG" }
          },
        },
        colors = {
          info = { "#66D9EF" },
          hint = { "#CCCCCC" },
          error = { "#F92672" },
          warning = { "#FD971F" },
          perf = { "#A6E22E" },
          default = { "Identifier", "#AE81FF" },
          test = { "#7E8E91" }
        },
        merge_keywords = true,
        highlight = {
          multiline = false,
          before = "",
          keyword = "bg",
          after = "",
          comments_only = true
        },
      })
    end
  },

  -- The fastest colorizer, #a6e22e
  {
    "NvChad/nvim-colorizer.lua",
    lazy = false,
    config = function()
      require("colorizer").setup({
        filetypes = { "*" },
        user_default_options = {
          RGB = false,
          names = false
        }
      })
    end
  },

  -- Indent guides for Neovim
  {
    "lukas-reineke/indent-blankline.nvim",
    lazy = false,
    config = function()
      require("ibl").setup({
        indent = {
          char = "▏" -- or "│"
        },
        scope = {
          show_start = false,
          show_end = false,
          injected_languages = true
        },
        exclude = {
          filetypes = {
            "TelescopePrompt", "TelescopeResults",
            "alpha",
            "help",
            "lazy",
            "lspinfo",
            "terminal",
            "org",
            ""
          },
          buftypes = {
            "terminal", "nofile", "quickfix", "prompt"
          }
        }
      })
    end
  },

  -- Rename tabs
  {
    "gcmt/taboo.vim"
  }
}
