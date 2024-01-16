return {
    {
        "robitx/gp.nvim",
        config = function()
            require("gp").setup({
                openai_api_key = os.getenv("OPENAI_API_KEY"),
                openai_api_endpoint = "https://api.openai.com/v1/chat/completions",
                cmd_prefix = "Gp",

                agents = {
                    {
                        name = "ChatGPT4",
                        chat = true,
                        command = false,
                        model = { model = "gpt-4-1106-preview", temperature = 1.1, top_p = 1 },
                        system_prompt = "You are a general AI assistant.\n\n"
                            .. "The user provided the additional info about how they would like you to respond:\n\n"
                            .. "- If you're unsure don't guess and say you don't know instead.\n"
                            .. "- Ask question if you need clarification to provide better answer.\n"
                            .. "- Think deeply and carefully from first principles step by step.\n"
                            .. "- Zoom out first to see the big picture and then zoom in to details.\n"
                            .. "- Use Socratic method to improve your thinking and coding skills.\n"
                            .. "- Don't elide any code from your output if the answer requires coding.\n"
                            .. "- Take a deep breath; You've got this!\n",
                    },
                    {
                        name = "ChatGPT3-5",
                        chat = true,
                        command = false,
                        model = { model = "gpt-3.5-turbo-1106", temperature = 1.1, top_p = 1 },
                        system_prompt = "You are a general AI assistant.\n\n"
                            .. "The user provided the additional info about how they would like you to respond:\n\n"
                            .. "- If you're unsure don't guess and say you don't know instead.\n"
                            .. "- Ask question if you need clarification to provide better answer.\n"
                            .. "- Think deeply and carefully from first principles step by step.\n"
                            .. "- Zoom out first to see the big picture and then zoom in to details.\n"
                            .. "- Use Socratic method to improve your thinking and coding skills.\n"
                            .. "- Don't elide any code from your output if the answer requires coding.\n"
                            .. "- Take a deep breath; You've got this!\n",
                    },
                    {
                        name = "CodeGPT4",
                        chat = false,
                        command = true,
                        model = { model = "gpt-4-1106-preview", temperature = 0.8, top_p = 1 },
                        system_prompt = "You are an AI working as a code editor.\n\n"
                            .. "Please AVOID COMMENTARY OUTSIDE OF THE SNIPPET RESPONSE.\n"
                            .. "START AND END YOUR ANSWER WITH:\n\n```",
                    },
                    {
                        name = "CodeGPT3-5",
                        chat = false,
                        command = true,
                        model = { model = "gpt-3.5-turbo-1106", temperature = 0.8, top_p = 1 },
                        system_prompt = "You are an AI working as a code editor.\n\n"
                            .. "Please AVOID COMMENTARY OUTSIDE OF THE SNIPPET RESPONSE.\n"
                            .. "START AND END YOUR ANSWER WITH:\n\n```",
                    },
                },
                chat_dir = os.getenv("HOME") .. "/Workspaces/LLM/GPT",

                chat_user_prefix = "## >> (You):",
                chat_assistant_prefix = { "## << (AI: ", "[{{agent}}]):" },
                command_prompt_prefix_template = "## ~ (AI: {{agent}}):",

                chat_topic_gen_model = "gpt-3.5-turbo-16k",
                chat_topic_gen_prompt = "Summarize the topic of our conversation above"
                    .. " in two or three words. Respond only with those words.",

                -- whisper_dir = (os.getenv("TMPDIR") or os.getenv("TEMP") or "/tmp") .. "/gp_whisper",
                -- whisper_silence = "1.75",
                -- whisper_max_time = "05:00",
                -- whisper_tempo = "1.75",
                -- whisper_language = "en",
            })
                -- hooks = {
                --     UnitTests = function(gp, params)
                --         local template = "I have the following code from {{filename}}:\n\n"
                --             .. "```{{filetype}}\n{{selection}}\n```\n\n"
                --             .. "Please respond by writing table driven unit tests for the code above."
                --         gp.Prompt(params, gp.Target.enew("markdown"), nil, gp.config.command_model,
                --             template, gp.config.command_system_prompt)
                --     end,
                --     Explain = function(gp, params)
                --         local template = "I have the following code from {{filename}}:\n\n"
                --             .. "```{{filetype}}\n{{selection}}\n```\n\n"
                --             .. "Please respond by explaining the code above."
                --         gp.Prompt(params, gp.Target.popup, nil, gp.config.command_model,
                --             template, gp.config.chat_system_prompt)
                --     end,
                --     CodeReview = function(gp, params)
                --         local template = "I have the following code from {{filename}}:\n\n"
                --             .. "```{{filetype}}\n{{selection}}\n```\n\n"
                --             .. "Please analyze for code smells and suggest improvements."
                --         gp.Prompt(params, gp.Target.enew("markdown"), nil, gp.config.command_model,
                --             template, gp.config.command_system_prompt)
                --     end
                -- }

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
