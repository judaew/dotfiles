return {
    {
        "robitx/gp.nvim",
        config = function()
            require("gp").setup({
                chat_dir = "/Users/judaew/Workspaces/LLM/GPT",
                chat_model = { model = "gpt-3.5-turbo-16k", temperature = 0.7, top_p = 1 },
                chat_user_prefix = "## >> (You):",
                chat_assistant_prefix = "## << (AI):",
                chat_topic_gen_model = { "gpt-3.5-turbo", temperature = 0.7, top_p = 1 },
                command_prompt_prefix = "~ (AI): ",
                command_model = { model = "gpt-3.5-turbo-16k", temperature = 0.7, top_p = 1 },

                hooks = {
                    UnitTests = function(gp, params)
                        local template = "I have the following code from {{filename}}:\n\n"
                            .. "```{{filetype}}\n{{selection}}\n```\n\n"
                            .. "Please respond by writing table driven unit tests for the code above."
                        gp.Prompt(params, gp.Target.enew, nil, gp.config.command_model,
                            template, gp.config.command_system_prompt)
                    end,
                    Explain = function(gp, params)
                        local template = "I have the following code from {{filename}}:\n\n"
                            .. "```{{filetype}}\n{{selection}}\n```\n\n"
                            .. "Please respond by explaining the code above."
                        gp.Prompt(params, gp.Target.popup, nil, gp.config.command_model,
                            template, gp.config.chat_system_prompt)
                    end,
                }
            })

            local map = function(mode, keys, cmd, desc)
                if desc then
                    desc = "GPT: " .. desc
                end
                vim.keymap.set(mode, keys, cmd, { desc = desc, noremap = true, silent = true, nowait = true })
            end

            -- Chat commands
            map("n", "<Leader>gc", "<cmd>GpChatNew<cr>", "New Chat")
            map("n", "<Leader>gt", "<cmd>GpChatToggle<cr>", "Toggle Popup Chat")
            map("n", "<Leader>gf", "<cmd>GpChatFinder<cr>", "Chat Finder")

            map("v", "<Leader>gc", ":<C-u>'<,'>GpChatNew<cr>", "Visual Chat New")
            map("v", "<Leader>gt", ":<C-u>'<,'>GpChatToggle<cr>", "Visual Popup Chat")

            -- Prompt commands
            map("n", "<Leader>gr", "<cmd>GpRewrite<cr>", "Inline Rewrite")
            map("n", "<Leader>ga", "<cmd>GpAppend<cr>", "Append")
            map("n", "<Leader>gb", "<cmd>GpPrepend<cr>", "Prepend")
            map("n", "<Leader>ge", "<cmd>GpEnew<cr>", "Enew")
            map("n", "<Leader>gp", "<cmd>GpPopup<cr>", "Popup")

            map("v", "<Leader>gr", ":<C-u>'<,'>GpRewrite<cr>", "Visual Rewrite")
            map("v", "<Leader>ga", ":<C-u>'<,'>GpAppend<cr>", "Visual Append")
            map("v", "<Leader>gb", ":<C-u>'<,'>GpPrepend<cr>", "Visual Prepend")
            map("v", "<Leader>ge", ":<C-u>'<,'>GpEnew<cr>", "Visual Enew")
            map("v", "<Leader>gp", ":<C-u>'<,'>GpPopup<cr>", "Visual Popup")

            map({ "n", "v", "x" }, "<Leader>gs", "<cmd>GpStop<cr>", "Stop")
        end
    }
}
