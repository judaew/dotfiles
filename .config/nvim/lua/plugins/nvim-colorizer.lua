local M = {}

function M.config()
    require("colorizer").setup({
        filetypes = { "*"},
        user_default_options = {
            RGB = false,
            names = false
        }
    })
end

return M
