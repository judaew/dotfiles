return {
  -- Syntax highlighting for the nginx files
  {
    "fatih/vim-nginx",
    ft = { "nginx", "nginx.conf" }
  },

  -- Syntax highlighting for generic log files
  {
    "MTDL9/vim-log-highlighting",
    ft = { "log" }
  },

  -- Syntax highlighting for the Portfile files
  {
    dir = "~/Workspaces/github.com/judaew/macports.nvim",
    config = function()
      vim.g.macports_completefunc = 1
      vim.g.macports_snippets = 1
    end,
    ft = "Portfile"
  }
}
