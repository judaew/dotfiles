local key = require("utils/keymap")

local M = {}

function M.keys()
    -- Function keys
    key.set("n", "<F4>", "<cmd>GV<CR>", "Commit browser")
    key.set("n", "<S-F4>", "<cmd>GV!<CR>", "Commit browser")
end

return M
