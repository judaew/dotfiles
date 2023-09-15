local M = {}

function M.config()
    require("sniprun").setup({
        binary_path="/opt/local/sbin/sniprun",
    })
end

local map = function(mode, keys, func, desc)
    if desc then
            desc = "Code runner: " .. desc
    end
    vim.keymap.set(mode, keys, func, { desc=desc, noremap=true })
end

function M.keys()
    -- Function keys
    map("n", "<F7>",   ":SnipRun<CR>",   "Run the lines")
    map("v", "<F7>",   ":SnipRun<CR>",   "Run the lines")
    map("n", "<S-F7>", ":%SnipRun<CR>",  "Run the buffer")
    map("n", "<F8>",   ":SnipClose<CR>", "Close")
    map("n", "<S-F8>", ":SnipReset<CR>", "Reset")
end

return M
