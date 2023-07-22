local key = require('utils/keymap')

local M = {}

function M.keys()
    -- Function keys
    -- See https://github.com/stevearc/overseer.nvim/blob/master/doc/reference.md#commands
    local f_keymaps_table = {
        { "<F5>",   ":OverseerRun<CR>",   "Run a task with make system" },
        { "<S-F5>", ":OverseerTaskAction<CR>", "Run a preset/temporary task" },
        { "<F6>",   ":OverseerToggle<CR>","Task status window" },
        { "<S-F6>", ":OverseerBuild<CR>", "Open a temporary task builder" }
    }
    key.bulk_set(f_keymaps_table, "n")
end

return M
