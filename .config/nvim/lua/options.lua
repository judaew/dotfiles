local options = {
    termguicolors = true,
    colorcolumn = "80",
    expandtab = true,
    tabstop = 4,
    shiftwidth = 4,
    mouse = "a",
    title = true,
    number = true,
    swapfile = false,
    splitright = true,
    splitbelow = true,
    hidden = true,
    showmode = false,
    ignorecase = true,
    conceallevel = 0,
    completeopt = {"menu", "preview", "menuone" },
    list = true,
    listchars = { trail = "·", tab = "│ " },
    pumheight=20,
    undofile = true,
    undodir = vim.fn.stdpath("cache") .. "/undo",
    wrap = false
}

for k,v in pairs(options) do
    vim.opt[k] = v
end

if vim.g.neovide then
    vim.opt.guifont = { "JetBrains Mono:h14" }
    vim.g.neovide_refresh_rate = 60
    vim.g.neovide_refresh_rate_idle = 1
    vim.g.neovide_remember_window_size = false
end

-- Fix netrw for macOS, see https://github.com/vim/vim/issues/4738
vim.cmd ([[
let g:netrw_http_cmd="open"
]])

-- Disable inline text and instead open a diagnostic window by <Leader>e
vim.diagnostic.config({
    virtual_text = false
})

-- Fix shada
vim.cmd ([[
set shada+=n~/.local/share/nvim/shada/main.shada
]])

-- Don't indent before html, head and body tags
vim.cmd ([[
let g:html_indent_autotags="html,head,body"
let g:indent_inner_html=1
]])

-- Python path
vim.cmd ([[
if has("mac")
    let g:python3_host_prog = "/opt/local/bin/python3"
else
    let g:python3_host_prog = "/usr/bin/python3"
endif
]])
