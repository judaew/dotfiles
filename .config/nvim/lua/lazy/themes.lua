return {
    {
        dir = "~/Workspaces/github.com/judaew/ronny.nvim",
        priority = 1000,
        config = function()
            vim.cmd.colorscheme("ronny")
            require("ronny").setup({})
        end,
    },
}
