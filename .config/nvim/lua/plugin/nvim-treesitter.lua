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
        "css",
        "diff",
        "dockerfile",
        "gdscript",
        "git_rebase",
        "gitattributes",
        "gitcommit",
        "go",
        "gomod",
        "html",
        "javascript",
        "json",
        "llvm",
        "lua",
        "markdown",
        "ninja",
        "proto",
        "python",
        "rust",
        "toml",
        "typescript",
        "yaml",
        "zig"
    }
}
