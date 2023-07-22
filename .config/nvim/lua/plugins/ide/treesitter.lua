local configs = require('nvim-treesitter.configs')

local M = {}

function M.config()
    configs.setup {
        ensure_installed = {
            "bash",
            "c",
            "cmake",
            "cpp",
            "css",
            "dockerfile",
            "git_rebase",
            "gitcommit",
            "go",
            "gomod",
            "html",
            "javascript",
            "json",
            "llvm",
            "lua",
            "make",
            "markdown",
            "ninja",
            "proto",
            "python",
            "rust",
            "sql",
            "toml",
            "typescript",
            "yaml",
            "zig"
        },
        -- Disable slow treesitter highlight for large files
        disable = function(buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
                return true
            end
        end,
        highlight = {
            enable = true,
            disable = {},
        },
        indent = {
            enable = true,
            disable = {
                "html",
                "lua",
                "python",
                "yaml"
            },
        },
        incremental_selection = {
            enable = true,
            keymaps = {
                init_selection = "<Leader>v",
                node_incremental = "<Leader>vi",
                scope_incremental = "<Leader>vs",
                node_decremental = "<Leader>vd",
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
                border = "none",
                floating_preview_opts = {},
                peek_definition_code = {
                    ["<leader>df"] = "@function.outer",
                    ["<leader>dF"] = "@class.outer",
                },
            },
            swap = {
                enable = true,
                swap_next = {
                    ["<leader>a"] = "@parameter.inner",
                },
                swap_previous = {
                    ["<leader>A"] = "@parameter.inner",
                }
            }
        }
    }

    -- Tree-sitter based folding
    vim.cmd([[
    set foldmethod=expr
    set foldexpr=nvim_treesitter#foldexpr()
    set nofoldenable
    ]])
end

return M
