return {
    {
        "fatih/molokai",
        enabled = true,
        priority = 1000,
        config = function()
            vim.opt.background="dark"
            vim.cmd.colorscheme("molokai")
            vim.g.molokai_origin=1
            vim.g.rehash=1
    end
    },
}
