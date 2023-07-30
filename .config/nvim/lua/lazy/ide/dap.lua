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
                config = function() require("plugins.ide.dap").dapui() end,
            }
        },
        config = function()
            require("plugins.ide.dap").dap()
            require("plugins.ide.dap").keys()
        end
    }
}
