local key = require("utils/keymap")

require("session_manager").setup({
    -- Open a session in the current directory if it exist
    autoload_mode = require('session_manager.config').AutoloadMode.CurrentDir,
    -- Create a session manually
    autosave_only_in_session = true
})

-- Keymaps
key.set("n", "<Leader>sms", "<cmd>SessionManager save_current_session<CR>", "Session Manager: Save")
key.set("n", "<Leader>smd", "<cmd>SessionManager delete_session<CR>", "Session Manager: Delete")
