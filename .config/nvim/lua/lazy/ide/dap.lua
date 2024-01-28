return {
    -- Debug Adapter Protocol
    {
        "mfussenegger/nvim-dap",
        -- Load only for configured languages
        ft = {"c", "cpp", "rust"},
        cmd = { "DapToggleBreakpoint", "DapContinue"},
        keys = {
            {"<Leader>db",  desc="DAP: Creates or removes a [b]reakpoint"},
            {"<Leader>dc",  desc="DAP: Start/[C]ontinue debugging"},
            {"<Leader>dsi", desc="DAP: [S]tep [i]nto a function or method"},
            {"<Leader>dso", desc="DAP: [S]tep [o]ver for <count> steps"},
            {"<Leader>dsq", desc="DAP: [S]tep out of a function or method"},
            {"<Leader>dsb", desc="DAP: [S]tep one step [b]ack"},
            {"<Leader>dr",  desc="DAP: [R]un to the current cursor"},
            {"<Leader>dq",  desc="DAP: Terminates the debug session"},

            {"<F10>",   desc="DAP: Toggle breakpoint"},
            {"<S-F10>", desc="DAP: Terminates the debug session"},
            {"<F11>",   desc="DAP: Start/Continue debugging"},
            {"<S-F11>", desc="DAP: Step out of a function or method"},
            {"<F12>",   desc="DAP: Step into a function or method"},
            {"<S-F12>", desc="DAP: Step over for [count] steps"}
        },
        dependencies = {
            -- plugin for UI
            {
                "rcarriga/nvim-dap-ui",
                config = function()
                    local dapui = require("dapui")

                    dapui.setup({
                        layouts = {
                            {
                                elements = {
                                    { id = "scopes",      size = 0.25 },
                                    { id = "breakpoints", size = 0.20 },
                                    { id = "stacks",      size = 0.25 },
                                    { id = "watches",     size = 0.30 },
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
                end,
            }
        },
        config = function()
            local dap   = require("dap")
            local dapui = require("dapui")

            -- See https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation#ccrust-via-lldb-vscode
            dap.adapters.lldb = {
                type = "executable",
                command = vim.fn.exepath("lldb-vscode-mp-16"),
                name = "lldb",
                env = {
                    LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES"
                },
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
                    runInTerminal = false,
                    externalTerminal = false,
                    args = {}
                }
            }

            dap.configurations.c = dap.configurations.cpp
            dap.configurations.rust = dap.configurations.cpp

            dap.listeners.after.event_initialized["dapui_config"] = function()
                dapui.open()
            end
            dap.listeners.before.event_terminated["dapui_config"] = function()
                dapui.close()
            end
            dap.listeners.before.event_exited["dapui_config"] = function()
                dapui.close()
            end
            require("plugins.ide.dap").dap()

            -- Keymaps
            -- See https://stackoverflow.com/a/52368238
            local map = function(keys, func, desc)
                if desc then
                    desc = "DAP: " .. desc
                end
                vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
            end

            map("<Leader>db",  dap.toggle_breakpoint, "Creates or removes a [b]reakpoint")
            map("<Leader>dc",  dap.continue,          "Start/[C]ontinue debugging")
            map("<Leader>dsi", dap.step_into,         "[S]tep [i]nto a function or method")
            map("<Leader>dso", dap.step_over,         "[S]tep [o]ver for <count> steps")
            map("<Leader>dsq", dap.step_out,          "[S]tep out of a function or method")
            map("<Leader>dsb", dap.step_back,         "[S]tep one step [b]ack")
            map("<Leader>dr",  dap.run_to_cursor,     "[R]un to the current cursor")
            map("<Leader>dq",  dap.terminate,         "Terminates the debug session")

            -- Function keys
            map("<F10>",   dap.toggle_breakpoint, "Toggle breakpoint")
            map("<S-F10>", dap.terminate,         "Terminates the debug session")
            map("<F11>",   dap.continue,          "Start/Continue debugging")
            map("<S-F11>", dap.step_out,          "Step out of a function or method")
            map("<F12>",   dap.step_into,         "Step into a function or method")
            map("<S-F12>", dap.step_over,         "Step over for [count] steps")
        end
    }
}
