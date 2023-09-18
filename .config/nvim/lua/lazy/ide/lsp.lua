return {
    -- Quickstart configs for Nvim LSP
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
    },
    {
        "ray-x/lsp_signature.nvim",
        after = "nvim-lspconfig",
        opts = {
            bind = true,
            handler_opts = { border = "shadow" },
            hint_enable = false
        },
        config = function(_, opts)
            require("lsp_signature").setup(opts)
        end
    }
}
