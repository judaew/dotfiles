local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    --- *** IDE section: Language Server Protocol, Debug Adapter Protocol,
    ---     Treesitter, Autocomplete/Intellisense, Snippets, Linting
    { import = "lazy.ide.lsp" },
    { import = "lazy.ide.dap" },
    { import = "lazy.ide.treesitter" },
    { import = "lazy.ide.intellisense" },
    { import = "lazy.ide.session_manager" },
    { import = "lazy.ide.task_runner"},
    { import = "lazy.ide.code_runner"},
    { import = "lazy.ide.workspaces" },

    --- *** Specific Language Support/Syntax Highlighting
    { import = "lazy.special_language_support" },

    --- *** Neovim Lua Development
    { import = "lazy.neovim_lua_development"},

    --- *** Utility/Special Features
    { import = "lazy.utility" },

    --- *** Movement, Fuzzy Finder, File Explorer
    { import = "lazy.movement" },
    { import = "lazy.fuzzy_finder" },
    { import = "lazy.file_explorer" },

    --- *** Editing Support/Text Manipulation
    { import = "lazy.editing_support" },

    --- *** Git
    { import = "lazy.git" },

    --- *** UI
    { import = "lazy.ui" },

    --- *** Themes
    { import = "lazy.themes" },
}, {
    performance = {
        cache = {
            enabled = true
        },
        rtp = {
            disabpled_plugins = {
                "gzip",
                "netrwPlugin",
                "rplugin",
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin"
            }
        }
    },
    ui = {
        icons = {
            cmd = "⌘",
            config = " ",
            event = " ",
            ft = " ",
            init = " ",
            keys = " ",
            plugin = " ",
            runtime = " ",
            source = " ",
            start = " ",
            task = " ",
            lazy = " ",
        },
    },
})
