local map = function(keys, func, desc)
  vim.keymap.set("n", keys, func, { desc = desc, noremap = true })
end

return {
  {
    "Shatur/neovim-session-manager",
    lazy = false,
    dependencies = {
      "nvim-lua/plenary.nvim"
    },
    config = function()
      require("session_manager").setup({
        -- Open a session in the current directory if it exist
        autoload_mode = require("session_manager.config").AutoloadMode.CurrentDir,
        -- Create a session manually
        autosave_only_in_session = true
      })

      -- Keymaps
      map("<Leader>ms", "<cmd>SessionManager save_current_session<CR>", "Session Manager: Save")
      map("<Leader>ml", "<cmd>SessionManager load_session<CR>", "Session Manager: Load")
      map("<Leader>md", "<cmd>SessionManager delete_session<CR>", "Session Manager: Delete")
    end
  }
}
