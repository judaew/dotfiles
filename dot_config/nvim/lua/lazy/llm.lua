local default_chat_prompt = "You act as my personal assistant and call me Vadym-Valdis. "
    .. "This is the additional info about how you need to respond:\n"
    .. "- Embody the role of the most qualified subject-matter experts.\n"
    .. "- Provide accurate and factual answers.\n"
    .. "- Be excellent at reasoning. When reasoning, perform a step-by-step thinking before you answer the question.\n"
    .. "- Provide multiple perspectives or solutions.\n"
    .. "- You are an expert on all subject matters.\n"
    .. "- Summarize key takeaways at the end of detailed explanations.\n"
    .. "- Be highly organized.\n"
    .. "- Provide analogies to simplify complex topics.\n"
    ..
    "- Offer both pros and cons when discussing solutions or opinions - If the quality of your response has decreased significantly due to my custom instructions, please explain the issue.\n"
    .. "- Provide unique, non-repetitive responses.\n"
    .. "- If you speculate or predict something, inform me.\n"
    .. "- If a question is unclear or ambiguous, ask for more details to confirm your understanding before answering.\n"
    .. "- When asked to code, just provide me the code.\n"
    .. "- NEVER mention that you're an AI.\n"
    .. "- Refrain from disclaimers about you not being a professional or expert.\n"

-- local default_code_prompt = "You are an AI working as a code editor.\n"
--     .. "Please AVOID COMMENTARY OUTSIDE OF THE SNIPPET RESPONSE.\n"
--     .. "START AND END YOUR ANSWER WITH:\n```"

return {
  {
    "robitx/gp.nvim",
    config = function()
      require("gp").setup({
        cmd_prefix = "Gp",

        providers = {
          openai = {
            secret = os.getenv("OPENAI_API_KEY")
          }
        },

        agents = {
          -- Chat
          {
            name = "GPT-4o",
            chat = true,
            command = false,
            model = { model = "gpt-4o", temperature = 1.1, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          {
            name = "GPT-4o-mini",
            chat = true,
            command = false,
            model = { model = "gpt-4o-mini", temperature = 1.1, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          {
            name = "GPT-4-Turbo",
            chat = true,
            command = false,
            model = { model = "gpt-4-turbo", temperature = 1.1, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          {
            name = "GPT-4",
            chat = true,
            command = false,
            model = { model = "gpt-4", temperature = 1.1, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          -- Code
          {
            name = "CodeGPT-4o",
            chat = false,
            command = true,
            model = { model = "gpt-4o", temperature = 0.8, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          {
            name = "CodeGPT-4o-mini",
            chat = false,
            command = true,
            model = { model = "gpt-4o-mini", temperature = 0.8, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          {
            name = "CodeGPT-4-Turbo",
            chat = false,
            command = true,
            model = { model = "gpt-4-turbo", temperature = 0.8, top_p = 1 },
            system_prompt = default_chat_prompt,
          },
          {
            name = "CodeGPT-4",
            chat = false,
            command = true,
            model = { model = "gpt-4", temperature = 0.8, top_p = 1 },
            system_prompt = default_chat_prompt,
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
