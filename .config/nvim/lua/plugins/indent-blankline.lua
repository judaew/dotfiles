local M = {}

function M.config()
        require("indent_blankline").setup({
        filetype_exclude = {
            "TelescopePrompt",
            "TelescopeResults",
            "alpha",
            "help",
            "lazy",
            "lspinfo",
            "org",
            "terminal",
            "",
        },
        buftype_exclude = { "terminal" },
        show_first_indent_level = true,
        show_current_context = true,
        })
end

return M
