local M = {}

function M.config()
        require('indent_blankline').setup({
        filetype_exclude = {
            "help",
            "terminal",
            "lazy",
            "lspinfo",
            "TelescopePrompt",
            "TelescopeResults",
            "alpha",
            "",
        },
        buftype_exclude = { "terminal" },
        show_first_indent_level = true,
        show_current_context = true,
        })
end

return M
