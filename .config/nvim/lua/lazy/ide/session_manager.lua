return {
    {
        "Shatur/neovim-session-manager",
        lazy = false,
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function()
            require("plugins.session_manager").config()
            require("plugins.session_manager").keys()
        end
    }
}
