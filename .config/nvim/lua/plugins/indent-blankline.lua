local M = {}

function M.config()
    require("ibl").setup({
        indent = {
            char = "▏" -- or "│"
        },
        scope = {
            show_start = false,
            show_end = false,
            injected_languages = true
        },
        exclude = {
            filetypes = {
                "TelescopePrompt",
                "TelescopeResults",
                "alpha",
                "help",
                "lazy",
                "lspinfo",
                "terminal",
                "org",
                ""
            },
            buftypes = {
                "terminal",
                "nofile",
                "quickfix",
                "prompt"
            }
        }
    })
end

return M
