return {
    {
        "nvim-telescope/telescope-ghq.nvim",
        config = function()
            require('plugins.telescope').workspaces()
            require('plugins.telescope').workspaces_keys()
        end
    }
}
