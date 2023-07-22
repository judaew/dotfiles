local M = {}

function M.config()
    require"colorizer".setup ({
        "*";
    }, {
        RGB = false;
        names = false
    })
end

return M
