local colors = {
    black   = "#232526",
    gray    = "#585858",
    white   = "#f8f8f2",
    cyan    = "#66d9ef",
    green   = "#a6e22e",
    orange  = "#ef5939",
    pink    = "#f92672",
    red     = "#ff0000",
    yellow  = "#e6db74"
}

local c = colors

-- see feline.nvim/lua/feline/providers/vi_mode.lua
local modes_table = {
    -- mode, name, foreground, background
    ["n"]     = {"NORMAL",     c.black,  c.cyan},
    ["no"]    = {"OP",         c.black,  c.cyan},
    ["nov"]   = {"OP",         c.black,  c.cyan},
    ["noV"]   = {"OP",         c.black,  c.cyan},
    ["no"]  = {"OP",         c.black,  c.cyan},
    ["niI"]   = {"NORMAL",     c.black,  c.cyan},
    ["niR"]   = {"NORMAL",     c.black,  c.cyan},
    ["niV"]   = {"NORMAL",     c.black,  c.cyan},
    ["v"]     = {"VISUAL",     c.black,  c.yellow},
    ["V"]     = {"V-LINES",    c.black,  c.yellow},
    [""]    = {"V-BLOCK",    c.black,  c.yellow},
    ["s"]     = {"SELECT",     c.black,  c.yellow},
    ["S"]     = {"SELECT",     c.black,  c.yellow},
    [""]    = {"S-BLOCK",    c.black,  c.yellow},
    ["i"]     = {"INSERT",     c.black,  c.green},
    ["ic"]    = {"INSERT",     c.black,  c.green},
    ["ix"]    = {"INSERT",     c.black,  c.green},
    ["R"]     = {"REPLACE",    c.black,  c.pink},
    ["Rc"]    = {"REPLACE",    c.black,  c.pink},
    ["Rv"]    = {"R-VIRTUAL",  c.black,  c.pink},
    ["Rx"]    = {"REPLACE",    c.black,  c.pink},
    ["c"]     = {"COMMAND",    c.black,  c.cyan},
    ["cv"]    = {"COMMAND",    c.black,  c.cyan},
    ["ce"]    = {"COMMAND",    c.black,  c.cyan},
    ["r"]     = {"HIT-ENTER",  c.black,  c.cyan},
    ["rm"]    = {"--MORE",     c.black,  c.cyan},
    ["r?"]    = {":CONFIRM",   c.black,  c.blue},
    ["!"]     = {"SHELL",      c.black,  c.green},
    ["t"]     = {"TERM",       c.black,  c.green},
    ["null"]  = {"NONE",       c.white,  c.yellow},
}

local function get_vi_mode_name_or_color(str)
    local current_mode = modes_table[vim.api.nvim_get_mode().mode]

    if (str == "name") then
        return current_mode[1]
    elseif (str == "fg") then
        return current_mode[2]
    elseif (str == "bg") then
        return current_mode[3]
    end
end

local comps = {
    vi_mode = {
        active = {
            provider = function()
                return " " .. get_vi_mode_name_or_color("name") .. " "
            end,
            hl = function()
                local val = {
                    fg = get_vi_mode_name_or_color("fg"),
                    bg = get_vi_mode_name_or_color("bg"),
                    style = "bold"
                }
                return val
            end,
        },
        inactive = {
            provider = function() return " NONE " end,
            hl = {
                fg = colors.black,
                bg = colors.yellow,
                style = "bold"
            }
        }
    },
    file = {
        info = {
            provider = {
                name = "file_info",
                opts = {
                    file_readonly_icon = "RO: ",
                    file_modified_icon = "+"
                }
            },
            icon = "",
            hl = {fg = colors.white, bg = colors.gray}
        },
        type = {
            provider = {
                name = "file_type",
                opts = {
                    case = "lowercase"
                },
            },
            hl = {fg = colors.black, bg = colors.pink},
            left_sep = " "
        }
    },
    line = {
        position = {
            provider = "position",
            hl = {bg = colors.gray},
            left_sep = {str = " ", hl = {bg = colors.gray}},
            right_sep = {str = " ", hl = {bg = colors.gray}}
        },
        percentage = {
            provider = "line_percentage",
            hl = {bg = colors.gray},
            right_sep = {str= " ", hl = {bg = colors.gray}}
        }
    },
    git_branch = {
        provider = "git_branch",
        left_sep = " ",
        icon = "",
    },
    lsp = {
        errors = {provider = "diagnostic_errors"},
        warnings = {provider = "diagnostic_warnings"},
        info = {provider = "diagnostic_info"},
        hints = {provider = "diagnostic_hints"}
    }
}

local components = {
    active = {
        { -- Left
            comps.vi_mode.active,
            comps.file.info,
            comps.git_branch
        },

        { -- Middle
        },

        { -- Right
            comps.lsp.errors,
            comps.lsp.warnings,
            comps.lsp.info,
            comps.lsp.hints,
            comps.file.type,
            comps.line.position,
            comps.line.percentage
        }
    },

    inactive = {
        { -- Left
            comps.vi_mode.inactive,
            comps.file.info
        },

        { -- Middle
        },

        { -- Right
            comps.file.type,
            comps.line.position,
            comps.line.percentage
        }
    }
}

require"feline".setup {
    colors = { bg = colors.black, fg = colors.white },
    components = components
}

-- local winbar_components = {
--     active = {
--         { -- Left
--         },
--         { -- Middle
--         },
--         { -- Right
--         }
--     },
--     inactive = {
--         { -- Left
--         },
--         { -- Middle
--         },
--         { -- Right
--         }
--     }
-- }
--
-- require"feline".winbar.setup {
--     colors = { bg = colors.black, fg = colors.white },
--     components = winbar_components
-- }
