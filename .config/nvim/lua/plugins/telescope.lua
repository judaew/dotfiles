local key = require("utils/keymap")
local telescope = require("telescope")
local builtin = require("telescope.builtin")
local themes = require("telescope.themes")
local actions = require "telescope.actions"

telescope.setup {
    pickers = {
        buffers = {
            mappings = {
                i = {
                    ["<C-d>"] = actions.delete_buffer + actions.move_to_top,
                }
            }
        }
    }
}

-- Enable telescope fzf native, if installed
pcall(telescope.load_extension, "fzf")

-- Keymaps
local keymaps_table = {
    { "<Leader>?", builtin.oldfiles, "[?] Find recently opened files" },
    { "<Leader><space>", builtin.buffers, "[ ] Find existing buffers" },
    { "<Leader>/", function()
        -- You can pass additional configuration to telescope to change theme,
        -- layout, etc.
        builtin.current_buffer_fuzzy_find(
            themes.get_dropdown {
                winblend = 10,
                previewer = false,
        }) end, "[/] Fuzzily search in current buffer" },
    { "<Leader>sf", builtin.find_files,  "[S]earch [F]iles" },
    { "<Leader>sh", builtin.help_tags,   "[S]earch [H]elp" },
    { "<Leader>sw", builtin.grep_string, "[S]earch current [W]ord" },
    { "<Leader>sg", builtin.live_grep,   "[S]earch by [G]rep" },
    { "<Leader>sd", builtin.diagnostics, "[S]earch [D]iagnostics" },
}
key.bulk_set(keymaps_table, "n")

-- Function keys
key.set("n", "<F3>", function() builtin.find_files({hidden=true, no_ignore=true}) end, "Find file")
key.set("n", "<S-F3>", builtin.oldfiles, "Recently opened files")
