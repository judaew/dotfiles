local hl = vim.api.nvim_set_hl
local severity = vim.diagnostic.severity

My = {}

local colors = {
    black  = "#1B1D1E",
    white  = "#F8F8F2",
    cyan   = "#66D9EF",
    green  = "#A6E22E",
    red    = "#F92672",
    yellow = "#E6DB74",
    blue   = "#266C98",
    gray   = "#585858"
}

local c = colors

-- See https://vi.stackexchange.com/a/22323
-- mode = { name, foreground, background }
local modes_table = {
    ["n"]     = { "NORMAL" },
    ["no"]    = { "OP" },
    ["nov"]   = { "OP" },
    ["noV"]   = { "OP" },
    ["no"]  = { "OP" },
    ["niI"]   = { "NORMAL" },
    ["niR"]   = { "NORMAL" },
    ["niV"]   = { "NORMAL" },
    ["v"]     = { "VISUAL" },
    ["V"]     = { "V-LINES" },
    [""]    = { "V-BLOCK" },
    ["s"]     = { "SELECT" },
    ["S"]     = { "SELECT" },
    [""]    = { "S-BLOCK" },
    ["i"]     = { "INSERT" },
    ["ic"]    = { "INSERT" },
    ["ix"]    = { "INSERT" },
    ["R"]     = { "REPLACE" },
    ["Rc"]    = { "REPLACE" },
    ["Rv"]    = { "R-VIRTUAL" },
    ["Rx"]    = { "REPLACE" },
    ["c"]     = { "COMMAND" },
    ["cv"]    = { "COMMAND" },
    ["ce"]    = { "COMMAND" },
    ["r"]     = { "HIT-ENTER" },
    ["rm"]    = { "MORE" },
    ["r?"]    = { "CONFIRM" },
    ["!"]     = { "SHELL" },
    ["t"]     = { "TERM" },
    ["null"]  = { "NONE" },
}

-- Set Vi Mode highlight groups
hl(0, "MyStatusLineModeNORMAL",    { fg=c.black, bg=c.cyan,   bold=true })
hl(0, "MyStatusLineModeOP",        { fg=c.black, bg=c.cyan,   bold=true } )
hl(0, "MyStatusLineModeVISUAL",    { fg=c.black, bg=c.yellow, bold=true })
hl(0, "MyStatusLineModeV-LINES",   { fg=c.black, bg=c.yellow, bold=true })
hl(0, "MyStatusLineModeV-BLOCK",   { fg=c.black, bg=c.yellow, bold=true })
hl(0, "MyStatusLineModeSELECT",    { fg=c.black, bg=c.yellow, bold=true })
hl(0, "MyStatusLineModeS-BLOCK",   { fg=c.black, bg=c.yellow, bold=true })
hl(0, "MyStatusLineModeINSERT",    { fg=c.black, bg=c.green,  bold=true })
hl(0, "MyStatusLineModeREPLACE",   { fg=c.black, bg=c.red,    bold=true })
hl(0, "MyStatusLineModeR-VIRTUAL", { fg=c.black, bg=c.red,    bold=true })
hl(0, "MyStatusLineModeCOMMAND",   { fg=c.black, bg=c.cyan,   bold=true })
hl(0, "MyStatusLineModeHIT-ENTER", { fg=c.black, bg=c.cyan,   bold=true })
hl(0, "MyStatusLineModeMORE",      { fg=c.black, bg=c.cyan,   bold=true })
hl(0, "MyStatusLineModeCONFIRM",   { fg=c.black, bg=c.blue,   bold=true })
hl(0, "MyStatusLineModeSHELL",     { fg=c.black, bg=c.green,  bold=true })
hl(0, "MyStatusLineModeTERM",      { fg=c.black, bg=c.green,  bold=true })
hl(0, "MyStatusLineModeNONE",      { fg=c.black, bg=c.yellow, bold=true })

-- Set all other highlight groups
hl(0, "MyStatusLineDiagnosticMessageError", { fg=c.red,    bg="#343434" })
hl(0, "MyStatusLineDiagnosticMessageWarn",  { fg=c.yellow, bg="#343434" })
hl(0, "MyStatusLineDiagnosticMessageInfo",  { fg=c.yellow, bg="#343434" })
hl(0, "MyStatusLineDiagnosticMessageHint",  { fg=c.white,  bg="#343434" })
hl(0, "MyStatusLineFileInfo", { fg=c.white, bg=c.gray })
hl(0, "MyStatusLineFileType", { fg=c.black, bg=c.red })
hl(0, "MyStatusLinePosition", { link="MyStatusLineFileInfo" })

local severity_icons = {
    [vim.diagnostic.severity.ERROR] = { name="Error", icon=" " },
    [vim.diagnostic.severity.WARN] =  { name="Warn", icon=" " },
    [vim.diagnostic.severity.INFO] =  { name="Info", icon=" " },
    [vim.diagnostic.severity.HINT] =  { name="Hint", icon=" " }
}

local function get_diagnostic()
    local line = vim.api.nvim_win_get_cursor(0)[1] - 1
    local col = vim.api.nvim_win_get_cursor(0)[2]

    local opts = { ['lnum'] = line }
    local diagnostics = vim.diagnostic.get(0, opts)

    for _, diagnostic in ipairs(diagnostics) do
        local start_col = diagnostic.col
        local end_col = diagnostic.end_col or start_col

        local str = "%#MyStatusLineDiagnosticMessage"

        if col >= start_col and col <= end_col then
            local diagnostic_info = severity_icons[diagnostic.severity]
            local window_width = vim.fn.winwidth(0)

            if diagnostic_info then
                local name = diagnostic_info.name
                local icon = diagnostic_info.icon
                str = str .. name .. "# " .. icon .. ": "
            end

            str = str .. "%#StatusLine#"

            if window_width < 80 then
                str = ""
            elseif window_width < 100 then
                str = str .. diagnostic.message:sub(1,25)
            elseif window_width < 120 then
                str = str .. diagnostic.message:sub(1,40)
            elseif window_width >= 120 then
                str = str .. diagnostic.message:sub(1,60)
            end

            return str
        end
    end

    return ""
end

function My.StatusLine()
    local focus = vim.g.statusline_winid == vim.fn.win_getid()
    local bufnr = vim.fn.bufnr()
    local reset = "%#StatusLine#"

    local statusline = {}

    --- mode
    local mode = vim.fn.mode()
    local mode_name = modes_table[mode][1]
    if focus then
        table.insert(statusline, "%#MyStatusLineMode" .. mode_name .. "#")
        table.insert(statusline, " " .. mode_name .. " " .. reset)
    else
        table.insert(statusline, "%#MyStatusLineModeNONE# NONE " .. reset)
    end
    table.insert(statusline, "%<")

    --- fileinfo
    table.insert(statusline, "%#MyStatusLineFileInfo#" .. " %t %M " .. reset)

    --- diagnostic error on the current line
    table.insert(statusline, get_diagnostic())

    --- separation point
    table.insert(statusline, "%=")

    --- diagnostic status
    local function get_diag_count(severity_var)
        return vim.tbl_count(vim.diagnostic.get(bufnr, severity and { severity = severity_var }))
    end

    local function get_severity_icon(severity_var, count)
        local icon_info = severity_icons[severity_var]
        if icon_info and count ~= 0 then
            return string.format(" %s%s", icon_info.icon, count)
        end
        return ""
    end

    local diagnostic_status = ""
    if next(vim.lsp.buf_get_clients(bufnr)) ~= nil then
        local lsp_errors =  get_diag_count(severity.ERROR)
        local lsp_warns = get_diag_count(severity.WARN)
        local lsp_info = get_diag_count(severity.INFO)
        local lsp_hints = get_diag_count(severity.HINT)

        diagnostic_status = diagnostic_status .. get_severity_icon(severity.ERROR, lsp_errors)
        diagnostic_status = diagnostic_status .. get_severity_icon(severity.WARN, lsp_warns)
        diagnostic_status = diagnostic_status .. get_severity_icon(severity.INFO, lsp_info)
        diagnostic_status = diagnostic_status .. get_severity_icon(severity.HINT, lsp_hints)
        diagnostic_status = diagnostic_status .. " "
    end

    if focus then
        table.insert(statusline, diagnostic_status)
    end

    --- filetype
    table.insert(statusline, "%#MyStatusLineFileType#" .. " %Y " .. reset)

    --- position
    table.insert(statusline, "%>")
    table.insert(statusline, "%#MyStatusLinePosition#" .. " %l:%L %P " .. reset)

    return table.concat(statusline)
end

vim.o.statusline = "%!v:lua.My.StatusLine()"
