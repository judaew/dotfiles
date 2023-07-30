local M = {}

function M.config()
    require("neogen").setup({
        snippet_engine = "vsnip",
        languages = {
            lua = {
                template = {
                    annotation_convention = "ldoc"
                }
            }
        }
    })
end

local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

function M.keys()
    map("<Leader>cc", function() require("neogen").generate({}) end,
        "Neogen Comment")
end

return M
