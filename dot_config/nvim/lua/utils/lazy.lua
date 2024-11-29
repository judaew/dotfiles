local M = {}

function M.git_load(plugin)
    -- load plugin only when a git file is opened
    vim.api.nvim_create_autocmd({ "BufRead" }, {
        callback = function()
            vim.fn.system("git -C " .. '"' .. vim.fn.expand "%:p:h" .. '"' .. " rev-parse")
            if vim.v.shell_error == 0 then
            vim.schedule(function()
                require("lazy").load { plugins = { plugin } }
            end)
        end
    end})
end

return M
