return {
    {
        "natecraddock/workspaces.nvim",
        lazy = false,
        config = function()
            require('plugins.telescope').workspaces()
            require('plugins.telescope').workspaces_keys()
        end
    },
}
