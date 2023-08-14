return {
    -- Orgmode clone written in Lua
    {
        "nvim-orgmode/orgmode",
        config = function()
            require("orgmode").setup_ts_grammar()
            require("orgmode").setup({
                org_agenda_files = "~/Org/**/*",
                org_default_notes_file = "~/Org/Notes.org",
                org_todo_keywords = {
                    "TODO", "WAITING", "|", "DONE", "DELEGATED"
                },
                org_todo_keyword_faces = {
                    TODO = ":foreground #65D9EF :weight bold",
                    WAITING = ":foreground #E2DB74 :weight bold",
                    DONE = ":foreground #A7E22E :weight bold",
                    DELEGATED = ":foreground #F92572 :weight bold"
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
                    j = {
                        description = "Journal",
                        template = "* %<%Y-%m-%d> %?\n  %u",
                        target = "~/Org/Journal.org"
                    },
                    e =  'Event',
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
                }
            })
        end
    },

    -- Multipurpose key to do various action in org files
    {
        "andreadev-it/orgmode-multi-key",
        config = function()
            require("orgmode-multi-key").setup()
        end
    }
}
