return {
    {
        "stevearc/overseer.nvim",
        -- Since `keys` calls a command (:), not a function, `cmd` is required.
        cmd = { "OverseerRun", "OverseerTaskAction", "OverseerToggle", "OverseerToggle" },
        keys = function() require("plugins.task_runner").keys() end,
        dependencies = {
            "stevearc/dressing.nvim"
        },
        config = function() require("overseer").setup() end
    }
}
