return {
    {
        "fatih/molokai",
        enabled = true,
        priority = 1000,
        config = function() vim.cmd ([[
            set background=dark
            colorscheme molokai
            let g:molokai_origin=1
            let g:rehash=1
        ]]) end
    },
}
