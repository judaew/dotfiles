return {
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            {
                "nvim-treesitter/nvim-treesitter-textobjects",
                after = "nvim-treesitter"
            }
        },
        build = ":TSUpdate",
        config = function() require("plugins.ide.treesitter").config() end
    }
}
