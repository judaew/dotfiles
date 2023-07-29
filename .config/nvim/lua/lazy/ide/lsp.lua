return {
    {
        "neovim/nvim-lspconfig",
        lazy = false,
        dependencies = {
            { "weilbith/nvim-code-action-menu" },
            { "kosayoda/nvim-lightbulb" }
        },
        config = function()
            require("plugins.ide.lsp").lsp()
            require("plugins.ide.lsp").nvim_lightbulb()
        end
    }
}
