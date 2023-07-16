local api = vim.api
local keymap = vim.keymap.set
local sb = require("utils/searchBuffer")

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
keymap("n", "<F9>", function() toggleTagBar("nvim_lsp") end,
    { desc = "Toggle Tagbar (finder: LSP)" })
keymap("n", "<S-F9>", function() toggleTagBar("ctags") end,
    { desc = "Toggle Tagbar (finder: Ctags)" })
