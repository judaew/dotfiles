local dap = require("dap")
local dapui = require("dapui")

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- keymaps
keymap("n", "<F5>", require "dap".continue, opts)
keymap("n", "<F10>", require "dap".step_over, opts)
keymap("n", "<F11>", require "dap".step_into, opts)
keymap("n", "<F12>", require "dap".step_out, opts)
keymap("n", "<leader>b", require "dap".toggle_breakpoint, opts)

-- See https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation#ccrust-via-lldb-vscode
dap.adapters.lldb = {
    type = "executable",
    command = vim.fn.exepath("lldb-vscode-mp-15"),
    name = "lldb"
}

dap.configurations.cpp = {
    {
        name = "Launch",
        type = "lldb",
        request = "launch",
        program = function()
          return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
        end,
        cwd = "${workspaceFolder}",
        stopOnEntry = false,
        args = {},
        runInTerminal = false,
    }
}

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

dapui.setup({
    layouts = {
        {
            elements = {
                { id = "scopes", size = 0.25 },
                { id = "breakpoints", size = 0.20 },
                { id = "stacks", size = 0.25 },
                { id = "watches", size = 0.30 },
            },
            size = 50,
            position = "right",
        },
        {
            elements = { "repl" },
            size = 10,
            position = "bottom",
        },
    },
    windows = { indent = 1 },
})

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end

