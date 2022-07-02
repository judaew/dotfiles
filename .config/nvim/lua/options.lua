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
    undofile = true,
    undodir = "/Users/judaew/.cache/nvim/undo",
    wrap = false
}

for k,v in pairs(options) do
    vim.opt[k] = v
end

-- setup theme
vim.cmd ([[
set background=dark
colorscheme molokai
let g:molokai_origin=1
let g:rehash=1
]])