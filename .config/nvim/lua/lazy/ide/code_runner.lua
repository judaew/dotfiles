return {
    -- Code Runner
    {
        "michaelb/sniprun",
        -- Since `keys` calls a command (:), not a function, `cmd` is required.
        cmd = { "SnipRun", "SnipClose", "SnipReset" },
        keys = function() require("plugins.code_runner").keys() end,
        config = function() require("plugins.code_runner").config() end
    }
}
