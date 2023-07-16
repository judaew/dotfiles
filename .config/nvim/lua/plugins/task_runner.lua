local key = require("utils/keymap")


require "overseer".setup({
})

-- Function keys
-- See https://github.com/stevearc/overseer.nvim/blob/master/doc/reference.md#commands
local f_keymaps_table = {
    { "<F5>",   "<cmd>OverseerRun<CR>",   "Run a task with make system" },
    { "<S-F5>", "<cmd>OverseerTaskAction<CR>", "Run a preset/temporary task" },
    { "<F6>",   "<cmd>OverseerToggle<CR>","Task status window" },
    { "<S-F6>", "<cmd>OverseerBuild<CR>", "Open a temporary task builder" }
}
key.bulk_set(f_keymaps_table, "n")
