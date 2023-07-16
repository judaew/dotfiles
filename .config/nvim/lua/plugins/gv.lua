local key = require("utils/keymap")

-- Function keys
key.set("n", "<F4>", "<cmd>GV<CR>", "Commit browser")
key.set("n", "<S-F4>", "<cmd>GV!<CR>", "Commit browser")
