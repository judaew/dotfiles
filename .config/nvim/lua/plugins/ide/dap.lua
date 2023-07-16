local dap   = require("dap")
local dapui = require("dapui")
local key   = require("utils/keymap")

-- FIXME: Temporary solution until I create my module for this
local keymap = function(mode, keys, func, desc)
    if desc then
        desc = "DAP: " .. desc
    end

    vim.keymap.set(mode, keys, func,
        { noremap = true, silent = true, desc = desc })
end

-- Keymaps
-- See https://stackoverflow.com/a/52368238
keymap("n", "<Leader>bb",  dap.toggle_breakpoint, "Creates or removes a [b]reakpoint")
keymap("n", "<Leader>bc",  dap.continue,  "Start/[C]ontinue debugging")
keymap("n", "<Leader>bsi", dap.step_into, "[S]tep [i]nto a function or method")
keymap("n", "<Leader>bso", dap.step_over, "[S]tep [o]ver for <count> steps")
keymap("n", "<Leader>bsq", dap.step_out,  "[S]tep out of a function or method")
keymap("n", "<Leader>bsb", dap.step_back, "[S]tep one step [b]ack")
keymap("n", "<Leader>br",  dap.run_to_cursor, "[R]un to the current cursor")
keymap("n", "<Leader>bq",  dap.terminate, "Terminates the debug session")

-- Function keys
local f_keymaps_table = {
    {"n", "<F10>",   dap.toggle_breakpoint, "DAP: Toggle breakpoint"},
    {"n", "<S-F10>", dap.terminate, "DAP: Terminates the debug session"},
    {"n", "<F11>",   dap.continue,  "DAP: Start/Continue debugging"},
    {"n", "<S-F11>", dap.step_out,  "DAP: Step out of a function or method"},
    {"n", "<F12>",   dap.step_into, "DAP: Step into a function or method"},
    {"n", "<S-F12>", dap.step_over, "DAP: Step over for [count] steps"}
}
key.bulk_set(f_keymaps_table)

-- See https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation#ccrust-via-lldb-vscode
dap.adapters.lldb = {
    type = "executable",
    command = vim.fn.exepath("lldb-vscode-mp-16"),
    name = "lldb"
}

dap.configurations.cpp = {
    {
        name = "Launch",
        type = "lldb",
        request = "launch",
        program = function()
          return vim.fn.input("Path to executable: " .. vim.fn.getcwd() .. "/")
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
