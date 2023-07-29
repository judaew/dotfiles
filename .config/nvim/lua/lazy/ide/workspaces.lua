return {
    {
        "natecraddock/workspaces.nvim",
        config = function()
            require('plugins.telescope').workspaces()
            require('plugins.telescope').workspaces_keys()
        end
    },
}
