return {
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    branch = "main",
    build = ":TSUpdate",
    config = function()
      require 'nvim-treesitter'.setup({
        ensure_install = {
          "bash",
          "c",
          "c_sharp",
          "cmake",
          "cpp",
          "css",
          "dockerfile",
          "git_rebase",
          "gitcommit",
          "go",
          "gomod",
          "html",
          "javascript",
          "json",
          "llvm",
          "lua",
          "make",
          "markdown",
          "markdown_inline",
          "ninja",
          "proto",
          "python",
          "rust",
          "sql",
          "toml",
          "typescript",
          "yaml",
          "zig"
        },
        ignore_install = { 'unsupported', "org" },
        auto_install = false,

        -- Disable slow treesitter highlight for large files
        disable = function(buf)
          local max_filesize = 100 * 1024         -- 100 KB
          local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
          if ok and stats and stats.size > max_filesize then
            return true
          end
        end,
        highlight = {
          enable = true,
          disable = {},
          additional_vim_regex_highlighting = { "org" },
        },
        indent = {
          enable = true,
          disable = {
            -- "html",
            -- "lua",
            -- "python",
            -- "yaml"
          },
        },
      })

      -- Tree-sitter based folding
      vim.cmd([[
            set foldmethod=expr
            set foldexpr=nvim_treesitter#foldexpr()
            set nofoldenable
            ]])
    end
  }
}
