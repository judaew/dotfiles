local api = vim.api
local sb  = require("utils.searchBuffer")

local M = {}

local function toggleTagBar(finder)
    local bufferName = "__vista__"
    local bufferExist = sb.byName(bufferName)

    if bufferExist then
        api.nvim_command(":Vista!")
    else
        api.nvim_command(":Vista " .. finder)
    end
end

local function toggleTagBar_lsp()   toggleTagBar("nvim_lsp") end
local function toggleTagBar_ctags() toggleTagBar("ctags") end

local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

function M.keys()
    -- Function keys
    map("<F9>",   toggleTagBar_lsp,    "Toggle Tagbar (finder: LSP)")
    map("<S-F9>", toggleTagBar_ctags,  "Toggle Tagbar (finder: Ctags)")
end

return M
