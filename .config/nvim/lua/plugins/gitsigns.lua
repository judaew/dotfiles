local gitsigns = require'gitsigns'

gitsigns.setup {
    -- make gitsigns look like vim-signify
    signs = {
        add          = {hl = 'DiffAdd'   , text = '+'},
        change       = {hl = 'DiffChange', text = '!'},
        delete       = {hl = 'DiffDelete', text = '_', show_count=true},
        topdelete    = {hl = 'DiffDelete', text = '‾', show_count=true},
        changedelete = {hl = 'DiffChange', text = '~', show_count=true},
    }
}
