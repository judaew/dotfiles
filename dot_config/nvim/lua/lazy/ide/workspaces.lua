local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { desc=desc, noremap=true})
end

return {
    {
        "nvim-telescope/telescope-ghq.nvim",
        config = function()
            pcall(require("telescope").load_extension, "ghq")

            map("<Leader>sp", ":Telescope ghq list<CR>", "[S]earch [P]roject")
        end
    }
}
