local M = {}

function M.config()
    require("todo-comments").setup({
        signs = true,
        sign_priority = 8,
        keywords = {
            TODO = { icon = " ", color = "info" },
            NOTE = { icon = " ", color = "hint", alt = { "INFO" }},
            FIX  = { icon = " ", color = "error", alt =
                { "FIXME", "BUG", "FIXIT", "ISSUE" }},
            HACK = { icon = " ", color = "warning" },
            WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" }},
            PERF = { icon = " ", color = "perf", alt =
                { "OPTIM", "PERFORMANCE", "OPTIMIZE" }},
            TEST = { icon = " ", color = "test", alt =
                { "TESTING", "PASSED", "FAILED" }},
        },
        colors = {
            info = { "#66D9EF" },
            hint = { "#CCCCCC" },
            error = { "#F92672" },
            warning = { "#FD971F" },
            perf = { "#A6E22E" },
            default = { "Identifier", "#AE81FF" },
            test = { "#7E8E91" }
        },
        merge_keywords = true,
        highlight = {
            multiline = false,
            before = "",
            keyword = "bg",
            after = "",
            comments_only = true
        },
    })
end

return M
