return {
    -- Dashboard / Startpage
    {
        "goolord/alpha-nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        event = "VimEnter",
        config = function()
            local alpha     = require("alpha")
            local dashboard = require("alpha.themes.dashboard")

            dashboard.section.header.val = {
                [[ NVIM — Start Page ]]
            }

            dashboard.section.buttons.val = {
                dashboard.button("i",     "󰈔  New file", ":ene <BAR> startinsert<CR>"),
                dashboard.button(", s p", "󰃀  Load workspaces", ":Telescope ghq list<CR>"),
                dashboard.button(", m l", "󰁯  Load session", "<cmd>SessionManager load_session<CR>"),
                dashboard.button(", s f", "  Search file", ":lua require('telescope.builtin').find_files({hidden=true, no_ignore=true})<CR>"),
                dashboard.button(", ?",   "󰈢  Find recently opened files", ":lua require('telescope.builtin').oldfiles()<CR>"),
                dashboard.button(", s g", "󰈬  Search by Grep", ":lua require('telescope.builtin').live_grep()<CR>"),
                dashboard.button(", f m", "󰪶  File manager", ":NvimTreeToggle<CR>"),
                dashboard.button("q",     "󰅚  Quit NVIM", ":qa<CR>"),
            }

            alpha.setup(dashboard.config)
        end
    }
}
