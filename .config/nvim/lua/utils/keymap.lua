local M = {}

---
-- Sets a key mapping using the `vim.keymap.set` function
--
-- @param mode (string)   The mode for the key mapping
-- @param key (string)    The key or key combination
-- @param func (function) The function to be called when the key is pressed
-- @param desc (string)   The description of the key mapping
-- @param opts (table)    Additional options for the key mapping (optional)
--
-- @usage
-- M.set("n", "<F10>", dap.toggle_breakpoint, "DAP: Toggle breakpoint")
-- M.set("n", "<F11>", dap.continue, "DAP: Start/Continue debugging", { silent = true })
--
function M.set(mode, key, func, desc, opts)
    if opts ~= nil then
        vim.keymap.set(mode, key, func, { desc = desc, table.unpack(opts) })
    else
        vim.keymap.set(mode, key, func, { desc = desc })
    end
end

---
-- Sets multiple key mappings using the `vim.keymap.set` function.
--
-- @param keymaps_table (table): The table containing the key mappings. Each
--   inner table should follow the structure:
--   - mode (string): optional if arg_mode is set
--   - key (string)
--   - func (function)
--   - desc (string): optional but recommended
--   - opts (string): optional
--
-- @param arg_mode (string): The mode for all key mappings (optional)
-- @param arg_desc (string): The description prefix for all key mappings (optional)
-- @param arg_opts (table):  The additional options for all key mappings (optional)
--
-- @usage
-- keymaps_table = {
--     { "<Leader>rn", vim.lsp.buf.rename,          "[R]e[n]ame" },
--     { "<Leader>D",  vim.lsp.buf.type_definition, "Type [D]efinition" },
-- }
-- M.bulk_set(keymaps_table, "n", "LSP: ", { buffer = bufnr })
--
function M.bulk_set(keymaps_table, arg_mode, arg_desc, arg_opts)
    for _, innerTable in ipairs(keymaps_table) do
        local i = 0
        local mode

        if arg_mode ~= nil then
            mode = arg_mode
            i = -1
        else
            mode = innerTable[i+1]
        end

        local key = innerTable[i+2]
        local func = innerTable[i+3]
        local desc = innerTable[i+4]
        local opts = innerTable[i+5]

        if arg_desc and desc then
            desc = arg_desc .. desc
        end

        if arg_opts and opts then
            if opts ~= nil then
                opts = arg_opts .. "," .. opts
            else
                opts = arg_opts
            end
        end

        if desc ~= nil and opts ~= nil then
            vim.keymap.set(mode, key, func, { desc = desc, table.unpack(opts) })
        elseif desc ~= nil then
            vim.keymap.set(mode, key, func, { desc = desc })
        elseif opts ~= nil then
            vim.keymap.set(mode, key, func, { table.unpack(opts) })
        else
            vim.keymap.set(mode, key, func)
        end
    end
end

return M
