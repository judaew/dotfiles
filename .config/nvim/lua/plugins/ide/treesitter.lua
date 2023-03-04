require'nvim-treesitter.configs'.setup {
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
        },
    highlight = {
        enable = true,
        disable = {},
    },
    indent = {
        enable = true,
        disable = { "yaml", "python" },
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = '<Leader>v',
            node_incremental = '<Leader>vi',
            scope_incremental = '<Leader>vs',
            node_decremental = '<Leader>vd',
        },
    },
    textobjects = {
        move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
                ["]m"] = "@function.outer",
                ["]]"] = "@class.outer",
            },
            goto_next_end = {
                ["]M"] = "@function.outer",
                ["]["] = "@class.outer",
            },
            goto_previous_start = {
                ["[m"] = "@function.outer",
                ["[["] = "@class.outer",
            },
            goto_previous_end = {
                ["[M"] = "@function.outer",
                ["[]"] = "@class.outer",
            },
        },
        lsp_interop = {
            enable = true,
            border = 'none',
            floating_preview_opts = {},
            peek_definition_code = {
                ["<leader>df"] = "@function.outer",
                ["<leader>dF"] = "@class.outer",
            },
        },
    },
}
