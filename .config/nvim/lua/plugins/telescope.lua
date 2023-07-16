local key = require("utils/keymap")
local telescope = require("telescope")
local telescope_builtin = require("telescope.builtin")
local telescope_themes = require("telescope.themes")

-- Enable telescope fzf native, if installed
pcall(telescope.load_extension, "fzf")

-- Keymaps
local keymaps_table = {
    { "<Leader>?", telescope_builtin.oldfiles, "[?] Find recently opened files" },
    { "<Leader><space>", telescope_builtin.buffers, "[ ] Find existing buffers" },
    { "<Leader>/", function()
        -- You can pass additional configuration to telescope to change theme,
        -- layout, etc.
        telescope_builtin.current_buffer_fuzzy_find(
            telescope_themes.get_dropdown {
                winblend = 10,
                previewer = false,
        }) end, "[/] Fuzzily search in current buffer" },
    { "<Leader>sf", telescope_builtin.find_files,  "[S]earch [F]iles" },
    { "<Leader>sh", telescope_builtin.help_tags,   "[S]earch [H]elp" },
    { "<Leader>sw", telescope_builtin.grep_string, "[S]earch current [W]ord" },
    { "<Leader>sg", telescope_builtin.live_grep,   "[S]earch by [G]rep" },
    { "<Leader>sd", telescope_builtin.diagnostics, "[S]earch [D]iagnostics" }
}
key.bulk_set(keymaps_table, "n")

-- Function keys
key.set("n", "<F3>", function() telescope_builtin.find_files({hidden=true, no_ignore=true}) end, "Find file")
key.set("n", "<S-F3>", telescope_builtin.oldfiles, "Recently opened files")
