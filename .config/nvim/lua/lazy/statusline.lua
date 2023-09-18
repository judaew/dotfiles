return {
    {
        "nvim-lualine/lualine.nvim",
        dependencies = {
            "nvim-tree/nvim-web-devicons", opts = true
        },
        opts = {
            options = {
                icons_enabled = true,
                component_separators = { left = "│", right = "│"},
                section_separators = { left = "", right = ""},
            },
            sections = {
                lualine_a = { "mode" },
                lualine_b = {
                    {
                        "filename",
                        path = 0,
                        shorting_target = 20,
                        symbols = {
                            modified = "+",
                            readonly = "-",
                            unnamed = "[No Name]",
                            newfile = "[New]"
                        }
                    }
                },
                lualine_c = { "diff" },
                lualine_x = {
                    {
                        "diagnostics",
                        colored = false,
                    }
                },
                lualine_y = { "%l:%v" }, -- location
                lualine_z = { "%P" }, -- progress
            }
        },
        config = function(_, opts) require("lualine").setup(opts) end
    }
}
