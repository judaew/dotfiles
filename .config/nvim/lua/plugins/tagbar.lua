local api = vim.api
local key = require("utils/keymap")
local sb  = require("utils/searchBuffer")

local function toggleTagBar(finder)
    local bufferName = "__vista__"
    local bufferExist = sb.byName(bufferName)

    if bufferExist then
        api.nvim_command(":Vista!")
    else
        api.nvim_command(":Vista " .. finder)
    end
end

-- Function keys
key.set("n", "<F9>",   function() toggleTagBar("nvim_lsp") end, "Toggle Tagbar (finder: LSP)")
key.set("n", "<S-F9>", function() toggleTagBar("ctags") end,  "Toggle Tagbar (finder: Ctags)")
