-- HACK: #104 Invalid in command-line window
-- See https://github.com/folke/todo-comments.nvim/issues/97
local hl = require("todo-comments.highlight")
local highlight_win = hl.highlight_win
hl.highlight_win = function(win, force)
    pcall(highlight_win, win, force)
end

require('todo-comments').setup {
    signs = false, highlight = { keyword = 'bg' }
}
