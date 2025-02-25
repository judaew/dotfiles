local map = function(keys, func, desc)
  vim.keymap.set("n", keys, func, { desc = desc, noremap = true })
end

return {
  -- Fuzzy Finder
  {
    "nvim-telescope/telescope.nvim",
    lazy = false,
    dependencies = {
      { "nvim-lua/plenary.nvim" },
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function() return vim.fn.executable "make" == 1 end,
        dependencies = { "nvim-telescope/telescope.nvim" },
        config = function()
          pcall(require("telescope").load_extension, "fzf")
        end
      },
    },
    -- See also "plugins/ide/lsp.lua"
    config = function()
      local actions = require("telescope.actions")

      require("telescope").setup({
        pickers = {
          buffers = {
            show_all_buffers = true,
            sort_mru = true,
            mappings = {
              i = {
                ["<C-d>"] = actions.delete_buffer + actions.move_to_top,
              }
            }
          }
        }
      })

      local builtin = require("telescope.builtin")
      local themes  = require("telescope.themes")

      local function current_buf_fuzzy_find()
        -- You can pass additional configuration to telescope to change theme,
        -- layout, etc.
        builtin.current_buffer_fuzzy_find(
          themes.get_dropdown {
            winblend = 10,
            previewer = false
          })
      end
      local function find_files()
        builtin.find_files({ hidden = true, no_ignore = true })
      end

      -- Keymaps main
      map("<Leader>?", builtin.oldfiles, "[?] Find recently opened files")
      map("<Leader><space>", builtin.buffers, "[ ] Find existing buffers")
      map("<Leader>/", current_buf_fuzzy_find, "[/] Fuzzily search in current buffer")
      map("<Leader>sf", find_files, "[S]earch [F]iles")
      map("<Leader>sh", builtin.help_tags, "[S]earch [H]elp")
      map("<Leader>sw", builtin.grep_string, "[S]earch current [W]ord")
      map("<Leader>sg", builtin.live_grep, "[S]earch by [G]rep")
      map("<Leader>sd", builtin.diagnostics, "[S]earch [D]iagnostics")
      map("<Leader>sr", builtin.resume, "[S]earch [R]esume/Continue")

      -- Function keys
      map("<F3>", find_files, "Find file")
      map("<S-F3>", builtin.oldfiles, "Recently opened files")
    end
  },

  -- Another fuzzy Finder
  {
    "junegunn/fzf.vim",
    lazy = false,
    dependencies = { "junegunn/fzf" }
  },
}
