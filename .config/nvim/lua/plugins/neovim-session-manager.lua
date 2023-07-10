require("session_manager").setup({
    -- Open a session in the current directory if it exist
    autoload_mode = require('session_manager.config').AutoloadMode.CurrentDir,
    -- Create a session manually
    autosave_only_in_session = true
})
