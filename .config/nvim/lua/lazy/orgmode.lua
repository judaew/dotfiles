return {
    -- Orgmode clone written in Lua
    {
        "nvim-orgmode/orgmode",
        config = function()
            require("orgmode").setup({
                org_agenda_files = "~/Org/**/*",
                org_default_notes_file = "~/Org/Notes.org",
                org_todo_keywords = {
                    "IDEA(i)",    -- maye someay
                    "TODO(t)",    -- doing later
                    "NEXT(n)",    -- doing now or soon
                    "|",
                    "DONE(d)",    -- done
                    "CANCELED(c)" -- canceled task
                    -- {"TODO", "WAITING", "|", "DONE", "DELEGATED"},
                },
                org_todo_keyword_faces = {
                    IDEA = ":foreground #CCCCCC :weight bold",
                    TODO = ":foreground #65D9EF :weight bold",
                    NEXT = ":foreground #E2DB74 :weight bold",
                    DONE = ":foreground #A7E22E :weight bold",
                    CANCELED = ":foreground #F92572 :weight bold"
                },
                -- Conceal bold/italic/underline/code/verbatim markers
                org_hide_emphasis_markers = true,
                -- See https://github.com/nvim-orgmode/orgmode/blob/6b6eb8eabbed4d95568fd1f5374a3dff7ed51a3b/doc/orgmode.txt#L594
                org_capture_templates = {
                    t = {
                        description = "Task",
                        template = "* TODO %?\n  %u",
                        target = "~/Org/Task.org"
                    },
                    T = {
                        description = "Todo",
                        template = "* TODO %?\n  %u",
                        target = "~/Org/Todo.org"
                    },
                    e =  "Event",
                    eo = {
                        description = "One-time",
                        template = "** %?\n   %T",
                        target = "~/Org/Calendar.org",
                        headline = "One-time"
                    },
                    er = {
                        -- See https://orgmode.org/manual/Repeated-tasks.html
                        description = "Recurring",
                        template = "** %?\n   %T",
                        target = "~/Org/Calendar.org",
                        headline = "Recurring"
                    }
                },
                org_blank_before_new_entry = { heading = true, plain_list_item = false },
                org_startup_folded = "showeverything"
            })
        end
    },

    -- Wiki and diary extension to orgmode filetype
    {
        "judaew/orgWiki.nvim",
        config = function()
            require("orgWiki").setup {
                -- TODO:
                wiki_path = { "~/Org/" },
                diary_path = "~/Org/Journal/",
            }
        end,
    },

    -- Background highlighting for headlines
    {
        "lukas-reineke/headlines.nvim",
        enabled = false,
        dependencies = "nvim-treesitter/nvim-treesitter",
        config = function()
            require("headlines").setup {
                org = {
                    -- headline_highlights = {
                    --     -- "Headline",
                    --     "@org.headline.level1",
                    --     "@org.headline.level2",
                    --     "@org.headline.level3",
                    --     "@org.headline.level4",
                    --     "@org.headline.level5",
                    --     "@org.headline.level6",
                    --     "@org.headline.level7",
                    --     "@org.headline.level8",
                    -- },
                    bullet_highlights = {
                        "@org.headline.level1",
                        "@org.headline.level2",
                        "@org.headline.level3",
                        "@org.headline.level4",
                        "@org.headline.level5",
                        "@org.headline.level6",
                        "@org.headline.level7",
                        "@org.headline.level8",
                    },
                    bullets = { "◉", "○", "✸", "✿" },
                    -- codeblock_highlight = "CodeBlock",
                    -- dash_highlight = "Dash",
                    -- dash_string = "-",
                    -- quote_highlight = "Quote",
                    -- quote_string = "┃",
                    fat_headlines = true,
                },
            }
        end
    }
}
