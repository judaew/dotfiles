local key = require("utils.keymap")

local M = {}

function M.config()
    require("sniprun").setup({
        binary_path="/opt/local/sbin/sniprun",
    })
end

function M.keys()
    -- Function keys
    local f_keymaps_table = {
        { { "n", "v" }, "<F7>",   ":SnipRun<CR>",   "Run the lines" },
        { "n",          "<S-F7>", ":%SnipRun<CR>",  "Run the buffer" },
        { "n",          "<F8>",   ":SnipClose<CR>", "Close" },
        { "n",          "<S-F8>", ":SnipReset<CR>", "Reset" }
    }
    key.bulk_set(f_keymaps_table, nil, "Code runner: ")
end

return M
