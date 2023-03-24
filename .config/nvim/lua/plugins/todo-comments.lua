require('todo-comments').setup {
    signs = true,
    sign_priority = 8,
    keywords = {
        TODO = { icon = " ", color = "info" },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" }},
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
        info = { "#84D6EC" },
        hint = { "#D3D3D3" },
        error = { "#EA3323" },
        warning = { "#F2A93B" },
        perf = { "#B3E053" },
        default = { "Identifier", "#7C3AED" },
        test = { "#768184" }
    },
    merge_keywords = true,
    highlight = {
        multiline = false,
        before = "",
        keyword = "bg",
        after = "",
        comments_only = true
    },
}
