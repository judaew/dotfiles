local M = {}

local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

function M.keys()
    -- Function keys
    map("<F4>",   "<cmd>GV<CR>",  "Commit browser")
    map("<S-F4>", "<cmd>GV!<CR>", "Commit browser")
end

return M
