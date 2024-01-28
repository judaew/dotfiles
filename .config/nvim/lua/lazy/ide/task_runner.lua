local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

return {
    {
        "stevearc/overseer.nvim",
        -- Since `keys` calls a command (:), not a function, `cmd` is required.
        cmd = { "OverseerRun", "OverseerTaskAction", "OverseerToggle", "OverseerToggle" },
        keys = function()
            -- Function keys
            -- See https://github.com/stevearc/overseer.nvim/blob/master/doc/reference.md#commands
            map("<F5>",   ":OverseerRun<CR>",   "Run a task with make system")
            map("<S-F5>", ":OverseerTaskAction<CR>", "Run a preset/temporary task")
            map("<F6>",   ":OverseerToggle<CR>","Task status window")
            map("<S-F6>", ":OverseerBuild<CR>", "Open a temporary task builder")
        end,
        dependencies = {
            "stevearc/dressing.nvim"
        },
        config = function() require("overseer").setup() end
    }
}
