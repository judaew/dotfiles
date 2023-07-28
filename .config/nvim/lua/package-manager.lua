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

local map = vim.keymap.set
require("lazy").setup({
    --- *** IDE section (LSP, DAP, linting, snippets)
    --- #############################################
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            -- plugin for better action menu
            { "weilbith/nvim-code-action-menu" },
            -- VSCode 💡 for neovim's built-in LSP
            {
                "kosayoda/nvim-lightbulb",
                config = function() require("plugins.ide.lsp").nvim_lightbulb() end
            }
        },
        config = function() require("plugins.ide.lsp").lsp() end
    },
    {
        "mfussenegger/nvim-dap",
        -- Load only for configured languages
        ft = {"c", "cpp", "rust"},
        cmd = { "DapToggleBreakpoint", "DapContinue"},
        keys = function() require("plugins.ide.dap").keys() end,
        dependencies = {
            -- plugin for UI
            {
                "rcarriga/nvim-dap-ui",
                config = function() require("plugins.ide.dap").dapui() end,
            }
        },
        config = function() require("plugins.ide.dap").dap() end
    },
    -- Plugin for automatically configures `lua-language-server` for
    -- Neovim config, Neovim runtime and plugin directories
    {
        "folke/neodev.nvim",
        ft = "lua",
        opts = {}
    },
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            {
                "nvim-treesitter/nvim-treesitter-textobjects",
                after = "nvim-treesitter"
            }
        },
        build = ":TSUpdate",
        config = function() require("plugins.ide.treesitter").config() end
    },
    -- Autocomplete/Intellisense
    {
        "hrsh7th/nvim-cmp",
        event = "InsertEnter",
        keys = { ":", "/", "?" },
        dependencies = {
            -- autopairing of (){}[] etc
            {
                "windwp/nvim-autopairs",
                config = function()
                    require("nvim-autopairs").setup({
                        fast_wrap = {},
                        disable_filetype = { "TelescopePrompt", "vim" },
                    })

                    -- setup cmp for autopairs
                    local cmp_autopairs = require("nvim-autopairs.completion.cmp")
                    require("cmp").event:on("confirm_done", cmp_autopairs.on_confirm_done())
                end,
            },
            -- cmp sources plugins
            {
                "hrsh7th/cmp-nvim-lsp",
                "hrsh7th/cmp-buffer",
                "hrsh7th/cmp-cmdline",
                "hrsh7th/cmp-nvim-lua",
                "hrsh7th/cmp-nvim-lsp-signature-help",
                "delphinus/cmp-ctags",
                "andersevenrud/cmp-tmux"
            },
            -- snippet plugin
            {
                "hrsh7th/vim-vsnip",
                dependencies = {
                    "hrsh7th/cmp-vsnip"
                },
                config = function() require("plugins.snippets").config() end
            }
            -- TODO: "paopaol/cmp-doxygen"
            -- TODO: "petertriho/cmp-git"
        },
        config = function() require("plugins.ide.intellisense").config() end
    },
    {
        "Shatur/neovim-session-manager",
        lazy = false,
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function()
            require("plugins.session_manager").config()
            require("plugins.session_manager").keys()
        end
    },
    {
        "stevearc/overseer.nvim",
        -- Since `keys` calls a command (:), not a function, `cmd` is required.
        cmd = { "OverseerRun", "OverseerTaskAction", "OverseerToggle", "OverseerToggle" },
        keys = function() require("plugins.task_runner").keys() end,
        dependencies = {
            "stevearc/dressing.nvim"
        },
        config = function() require("overseer").setup() end
    },
    {
        "michaelb/sniprun",
        -- Since `keys` calls a command (:), not a function, `cmd` is required.
        cmd = { "SnipRun", "SnipClose", "SnipReset" },
        keys = function() require("plugins.code_runner").keys() end,
        config = function() require("plugins.code_runner").config() end
    },

    -- *** Specific Language Support / Syntax Highlighting / Formatting
    { "gpanders/editorconfig.nvim" },
    {
        "fatih/vim-nginx",
        ft = {"nginx", "nginx.conf"}
    },
    {
        "MTDL9/vim-log-highlighting",
        ft = { "log" }
    },
    {
        dir = "~/Workspaces/github.com/judaew/macports.nvim",
        config = function()
            vim.g.macports_completefunc = 1
            vim.g.macports_snippets = 1
        end,
        ft = "Portfile"
    },

    -- *** Special Features
    {
        "tpope/vim-eunuch",
        lazy = false
    },

    -- *** Movement
    {
        "ethanholz/nvim-lastplace",
        config = function() require("plugins.nvim-lastplace").config() end
    },
    {
        -- TODO:
        -- * Show tag in the statusline, see:
        --   https://github.com/liuchengxu/vista.vim#show-the-nearest-methodfunction-in-the-statusline
        "liuchengxu/vista.vim",
        lazy = false,
        config = function() require("plugins.tagbar").keys() end
    },
    {
        "nvim-telescope/telescope.nvim",
        lazy = false,
        dependencies = {
            { "nvim-lua/plenary.nvim" },
            {
                "nvim-telescope/telescope-fzf-native.nvim",
                build = "make",
                cond = function() return vim.fn.executable "make" == 1 end,
                dependencies = { "nvim-telescope/telescope.nvim" },
                config = function() require("plugins.telescope").fzf_native() end
            },
            {
                "natecraddock/workspaces.nvim",
                after = "telescope.nvim",
                config = function()
                    require('plugins.telescope').workspaces()
                    require('plugins.telescope').workspaces_keys()
                end
            },

        },
        -- See also "plugins/ide/lsp.lua"
        config = function()
            require("plugins.telescope").telescope()
            require("plugins.telescope").telescope_keys()
        end
    },
    {
        "junegunn/fzf.vim",
        lazy = false,
        dependencies = { "junegunn/fzf" }
    },
    {
        "nvim-tree/nvim-tree.lua",
        lazy = false,
        config = function()
            require("plugins.file_manager").config()
            require("plugins.file_manager").keys()
        end
    },
    {
        "justinmk/vim-gtfo",
        keys = {
            {"gof", desc="$open: File manager (current file)"},
            {"goF", desc="$open: File manager (working directory)"},
            {"got", desc="$open: Terminal (current file)"},
            {"goT", desc="$open: Terminal (working directory)"}
        },
        config = function() vim.cmd([[
            let g:gtfo#terminals = { "mac": "kitty" }
        ]]) end
    },
    {
        "simnalamburt/vim-mundo",
        -- Plugin can't be lazy loaded
        lazy = false,
        config = function()
            map("n", "<Leader>u", ":MundoToggle<CR>",
                { desc="Toggle UndoTree (via Mundo)", noremap=true })
        end
    },
    {
        "t9md/vim-choosewin",
        lazy = false,
        config = function()
            map("n", "-", "<Plug>(choosewin)",
                { desc = "Choosewin", noremap=true })
        end
    },

    -- *** Text Manipulation
    {
        "numToStr/Comment.nvim",
        keys = {
            { "gcc", mode = "n",          desc = "Comment current line" },
            { "gc",  mode = { "n", "o" }, desc = "Comment linewise" },
            { "gc",  mode = "x",          desc = "Comment linewise (visual)" },
            { "gbc", mode = "n",          desc = "Comment current block" },
            { "gb",  mode = { "n", "o" }, desc = "Comment blockwise" },
            { "gb",  mode = "x",          desc = "Comment blockwise (visual)" },
        },
        config = function()
            require("Comment").setup({mappings={basic=true, extra=false}})
        end
    },
    {
        "AckslD/muren.nvim",
        cmd = { "MurenToggle", "MurenOpen", "MurenUnique" },
        keys = {
            {"<Leader>rm", function() require('muren.api').toggle_ui() end,
            desc="[M]ultiple [r]eplacements (toggle)"}
        },
        config = true
    },
    {
        "lyokha/vim-xkbswitch",
        lazy = false,
        init = function()
            vim.g.XkbSwitchEnabled = 1
            vim.g.XkbSwitchLib = "/opt/local/lib/libInputSourceSwitcher.dylib"
        end
    },
    { -- Detect and adjusts tabstop and shiftwidth automatically
        "tpope/vim-sleuth",
        lazy = false
    },

    -- *** Git
    {
        "tpope/vim-fugitive",
        lazy = false,
        dependencies = {
            "tpope/vim-rhubarb" -- For enable :Gbrowse
        }
    },
    {
        "junegunn/gv.vim",
        lazy = true,
        keys = {
            {"<F4>", desc="Commit browser"},
            {"<S-F4>", desc="Commit browser"}
        },
        init = function() require("utils.lazy").git_load("gv.vim") end,
        config = function() require("plugins.gv").keys() end
    },
    {
        "lewis6991/gitsigns.nvim",
        lazy = true,
        init = function() require("utils.lazy").git_load("gitsigns.nvim") end,
        dependencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require("plugins.gitsigns").config() end
    },
    {
        "NeogitOrg/neogit",
        enabled = false,
        dependencies = "nvim-lua/plenary.nvim",
        config = true
    },

    -- *** UI
    {
        "goolord/alpha-nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        event = "VimEnter",
        config = function() require("plugins.alpha-nvim").config() end
    },
    {
        "famiu/feline.nvim",
        lazy = false,
        config = function() require("plugins.feline").config() end
    },
    { "stevearc/dressing.nvim", opts = {} },
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        config = true
    },
    {
        "gennaro-tedesco/nvim-peekup",
        keys = {
            { '""',  desc = "Open peekup window" },
            { '"x',  desc = "Empty all registers" },
        }
    },
    {
        "folke/todo-comments.nvim",
        lazy = false,
        depencies = {
            "nvim-lua/plenary.nvim"
        },
        config = function() require("plugins.todo-comments").config() end
    },
    {
        "norcalli/nvim-colorizer.lua",
        config = function() require("plugins.nvim-colorizer").config() end
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        config = function() require("plugins.indent-blankline").config() end
    },

    -- *** Themes
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
}, {
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
