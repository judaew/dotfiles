return {
    {
        "nvim-treesitter/nvim-treesitter",
        lazy = false,
        dependencies = {
            {
                "nvim-treesitter/nvim-treesitter-textobjects",
                dependencies = "nvim-treesitter/nvim-treesitter"
            }
        },
        build = ":TSUpdate",
        config = function() require("plugins.ide.treesitter").config() end
    }
}
