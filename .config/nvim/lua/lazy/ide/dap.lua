return {
    -- Debug Adapter Protocol
    {
        "mfussenegger/nvim-dap",
        -- Load only for configured languages
        ft = {"c", "cpp", "rust"},
        cmd = { "DapToggleBreakpoint", "DapContinue"},
        keys = function() require("plugins.ide.dap").keys() end,
        dependencies = {
            -- plugin for UI
            {
                "rcarriga/nvim-dap-ui",
                config = function() require("plugins.ide.dap").dapui() end,
            }
        },
        config = function() require("plugins.ide.dap").dap() end
    }
}
