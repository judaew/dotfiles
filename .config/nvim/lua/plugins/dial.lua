local M = {}

function M.config()
    local augend = require("dial.augend")
    require("dial.config").augends:register_group({
        default = {
            augend.integer.alias.decimal, -- 0, 1, 2, 3, ...
            augend.integer.alias.hex,     -- 0x01, 0x1a1f, ...
            augend.date.alias["%Y/%m/%d"],-- 2023/07/31
            augend.date.alias["%Y-%m-%d"],-- 2023-07-31
            augend.constant.alias.bool,   -- true <-> false
            augend.semver.alias.semver,   -- 0.3.0, 1.22.1, 3.9.1, ...
            augend.constant.new({elements={"let", "const"}}), -- let <-> const
        },
    })
end

local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, expr=true, noremap=true})
end

function M.keys()
    map("<C-a>", function() return require("dial.map").inc_normal() end, "Increment")
    map("<C-x>", function() return require("dial.map").dec_normal() end, "Decrement")
end

return M
