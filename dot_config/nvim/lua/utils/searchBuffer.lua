local api = vim.api

local M = {}

---
-- Retrieves the full name of a buffer based on a partial match with the
-- specified name.
--
-- @param bufferName string:
--     The partial name of the buffer to search for.
-- @return string|nil:
--     The full name of the found buffer, or nil if the buffer is not found.
--
function M.byName(bufferName)
    local buffers = api.nvim_list_bufs()

    for _, buffer in ipairs(buffers) do
        local name = api.nvim_buf_get_name(buffer)
        local lowerName = string.lower(name)
        local lowerBufferName =  string.lower(bufferName)

        if string.find(lowerName, lowerBufferName, 1, true) then
            return name
        end
    end

    return nil
end

return M
