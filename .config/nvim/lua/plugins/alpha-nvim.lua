local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")

dashboard.section.header.val = {
    [[ NVIM — Start Page ]]
}

dashboard.section.buttons.val = {
    dashboard.button("e",   "󰈔  New file", ":ene <BAR> startinsert <CR>"),
    dashboard.button("f f", "  Find file", ":Telescope find_files hidden=true no_ignore=true<CR>"),
    dashboard.button("f h", "󰈢  Recently opened files", "<cmd>Telescope oldfiles<CR>"),
    dashboard.button("f g", "󰈬  Find word",  "<cmd>Telescope live_grep<cr>"),
    dashboard.button("f m", "󰃀  Jump to bookmarks", ":Marks<CR>"),
    dashboard.button("s l", "󰁯  Open last session", "<cmd>SessionManager load_last_session<CR>"),
    dashboard.button( "q",  "󰅚  Quit NVIM" , ":qa<CR>"),
}

alpha.setup(dashboard.config)
