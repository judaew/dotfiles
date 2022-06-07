require'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,
        disable = {},
    },
    indent = {
        enable = true,
        disable = { "yaml" },
    },
    ensure_installed = {
        "bash",
        "c",
        "cmake",
        "cpp",
        "dockerfile",
        "go",
        "gomod",
        "json",
        "llvm",
        "lua",
        "markdown",
        "ninja",
        "python",
        "toml",
        "yaml"
    }
}
