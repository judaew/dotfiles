local M = {}

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

-- see feline.nvim/lua/feline/providers/vi_mode.lua and
-- https://vi.stackexchange.com/a/22323
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

local function get_diagnostic(str)
    local line = vim.api.nvim_win_get_cursor(0)[1] - 1
    local col = vim.api.nvim_win_get_cursor(0)[2]

    local opts = { ['lnum'] = line }
    local diagnostics = vim.diagnostic.get(0, opts)

    for _, diagnostic in ipairs(diagnostics) do
        local start_col = diagnostic.col
        local end_col = diagnostic.end_col or start_col

        if col >= start_col and col <= end_col then
            if str == "message" then
                return diagnostic.message
            elseif str == "indicator" then
                return diagnostic.severity
            end
        end
    end

    return ""
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
    -- git_branch = {
    --     provider = "git_branch",
    --     left_sep = " ",
    --     icon = "",
    -- },
    lsp = {
        errors = {provider = "diagnostic_errors"},
        warnings = {provider = "diagnostic_warnings"},
        info = {provider = "diagnostic_info"},
        hints = {provider = "diagnostic_hints"}
    },
    diagnostic = {
        indicator = {
            provider = function()
                local severity = vim.diagnostic.severity
                local str

                if get_diagnostic("indicator") == severity.ERROR then
                    str = " E: "
                elseif get_diagnostic("indicator") == severity.WARN then
                    str = " W: "
                elseif get_diagnostic("indicator") == severity.INFO then
                    str = " I:"
                elseif get_diagnostic("indicator") == severity.HINT then
                    str = " H: "
                else
                    str = ""
                end

                return str
            end,
            hl = function()
                local severity = vim.diagnostic.severity
                local val = {}

                if get_diagnostic("indicator") == severity.ERROR then
                    val.fg = c.red
                elseif get_diagnostic("indicator") == severity.WARN then
                    val.fg = c.yellow
                elseif get_diagnostic("indicator") == severity.INFO then
                    val.fg = c.white
                elseif get_diagnostic("indicator") == severity.HINT then
                    val.fg = c.white
                end

                val.style = "bold"
                return val
            end
        },
        message = {
            provider = function() return get_diagnostic("message") end,
        }
    }
}

local components = {
    active = {
        { -- Left
            comps.vi_mode.active,
            comps.file.info,
            comps.diagnostic.indicator,
            comps.diagnostic.message
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

function M.config()
    require("feline").setup ({
        colors = { bg = colors.black, fg = colors.white },
        components = components
    })
end

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

return M
