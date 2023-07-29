return {
    {
        "neovim/nvim-lspconfig",
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
