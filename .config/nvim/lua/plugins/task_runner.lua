local keymap = vim.keymap.set

require "overseer".setup({
})

-- Function keys
-- See https://github.com/stevearc/overseer.nvim/blob/master/doc/reference.md#commands
keymap("n", "<F5>", "<cmd>OverseerRun<CR>", { desc = "Run a task with make system" })
keymap("n", "<S-F5>", "<cmd>OverseerTaskAction<CR>", { desc = "Run a preset/temporary task" })
keymap("n", "<F6>", "<cmd>OverseerToggle<CR>", { desc = "Task status window" })
keymap("n", "<S-F6>", "<cmd>OverseerBuild<CR>", { desc = "Open a temporary task builder" })
