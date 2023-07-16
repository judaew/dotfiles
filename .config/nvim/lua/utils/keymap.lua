local M = {}

function M.set(mode, key, func, desc, opts)
    local keymap = vim.keymap.set

    keymap(mode, key, func,
        { desc = desc, opts })

end

function M.bulk_set(keymaps_table)
    for _, innerTable in ipairs(keymaps_table) do
        local mode = innerTable[1]
        local key = innerTable[2]
        local func = innerTable[3]
        local desc = innerTable[4]
        local opts = innerTable[5]

        if opts == nil then
            vim.keymap.set(mode, key, func, { desc = desc })
        else
            vim.keymap.set(mode, key, func, { desc = desc, table.unpack(opts) })
        end
    end
end

return M
