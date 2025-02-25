local servers = {
  clangd = {
    cmd = { vim.fn.exepath("clangd"),
      "--compile-commands-dir=build",
      -- See https://github.com/hrsh7th/nvim-cmp/blob/3b9f28061a67b19cadc13946de981426a6425e4a/doc/cmp.txt#L948C43-L948C72
      "--header-insertion-decorators"
    }
  },
  gopls = {},
  lua_ls = {
    cmd = { vim.fn.exepath("lua-language-server") },
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        workspace = { checkThirdParty = false },
        completion = {
          workspaceWord = true,
        },
        hint = { enable = true },
        telemetry = { enable = false },
        maxPreload = 500,             -- 500 KB or ~10k lines per file
        preloadFileSize = 500         -- 500 KB or ~10k lines per file
      }
    }
  },
  pyright = {},
  bashls = {
    cmd = { vim.fn.exepath("bash-language-server"), "start" }
  },
  neocmake = {},
  efm = {},
  -- ltex = {
  --     ltex = {
  --         language = "en-US"
  --     }
  -- }
}

return {
  -- Quickstart configs for Nvim LSP
  {
    "neovim/nvim-lspconfig",
    lazy = false,
    dependencies = {
      { "weilbith/nvim-code-action-menu" },
      { "kosayoda/nvim-lightbulb" }
    },
    config = function()
      require("neodev").setup()

      -- local function insert_inlay_hint(client, bufnr)
      --     if client.server_capabilities.inlayHintProvider then
      --         vim.api.nvim_create_augroup("lsp_augroup", { clear = true })
      --
      --         vim.api.nvim_create_autocmd("InsertEnter", {
      --             buffer = bufnr,
      --             callback = function() vim.lsp.inlay_hint(bufnr, true) end,
      --             group = "lsp_augroup",
      --         })
      --         vim.api.nvim_create_autocmd("InsertLeave", {
      --             buffer = bufnr,
      --             callback = function() vim.lsp.inlay_hint(bufnr, false) end,
      --             group = "lsp_augroup",
      --         })
      --     end
      -- end

      local on_attach                                                    = function(client)
        --- Inlay Hints
        -- if client.name == "clangd" then
        --     M.insert_inlay_hint(client, bufnr)
        -- end

        --- Highlight current symbol
        if client.server_capabilities.documentHighlightProvider then
          vim.cmd([[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]])
          vim.cmd([[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]])
          vim.cmd([[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]])
        end
      end

      local telescope                                                    = require("telescope.builtin")
      local code_action                                                  = require("code_action_menu")

      local keys                                                         = function(ev)
        local map = function(keys, func, desc)
          if desc then
            desc = "LSP: " .. desc
          end
          vim.keymap.set("n", keys, func, { buffer = ev.buf, noremap = true, desc = desc })
        end

        -- Create a command `:Format` local to the LSP buffer
        vim.api.nvim_buf_create_user_command(ev.buf, "Format", function(_)
          vim.lsp.buf.format { async = true }
        end, { desc = "Format current buffer with LSP" })

        vim.api.nvim_buf_create_user_command(ev.buf, "WorkspaceListFolders", function(_)
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, { desc = "LSP: Workspace List Folders" })

        map("<Leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
        -- map("<Leader>ca", vim.lsp.buf.code_action,          "[C]ode [A]ction")
        map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
        -- map("gd",         vim.lsp.buf.definition,           "[G]oto [D]efinition")
        -- map("gr",         vim.lsp.buf.references,           "[G]oto [R]eferences")
        -- map("gI",         vim.lsp.buf.implementation,       "[G]oto [I]mplementation")
        -- map("<Leader>D",  vim.lsp.buf.type_definition,      "Type [D]efinition")
        map("<Leader>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
        map("<Leader>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
        map("<Leader>wl", ":WorkspaceListFolders<CR>", "[W]orkspace [L]ist Folders")

        -- See `:help K` for why this keymap
        map("K", vim.lsp.buf.hover, "Hover Documentation")
        map("<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")

        -- See commnad :Format above
        map("<Leader>fo", ":Format<CR>", "[Fo]rmat current buffer with LSP")

        --- Code Action Menu Plugin
        map("<Leader>ca", code_action.open_code_action_menu, "LSP: [C]ode [A]ction")

        --- Telescope Plugin
        map("gd", telescope.lsp_definitions, "[G]oto [D]efinition")
        map("gr", telescope.lsp_references, "[G]oto [R]eferences")
        map("gI", telescope.lsp_implementations, "[G]oto [I]mplementation")
        map("<Leader>D", telescope.lsp_type_definitions, "Type [D]efinition")
      end

      local capabilities                                                 = vim.lsp.protocol.make_client_capabilities()
      -- See https://github.com/nvim-lua/completion-nvim/issues/258
      capabilities.textDocument.completion.completionItem.snippetSupport = true
      -- Tell the server the capability of foldingRange
      -- See https://github.com/kevinhwang91/nvim-ufo
      capabilities.textDocument.foldingRange                             = {
        dynamicRegistration = false,
        lineFoldingOnly = true
      }
      -- nvim-cmp supports additional completion capabilities, so broadcast that
      -- to servers. See https://github.com/hrsh7th/cmp-nvim-lsp/tree/59224771f91b86d1de12570b4070fe4ad7cd1eeb#capabilities
      capabilities                                                       = require("cmp_nvim_lsp").default_capabilities(
      capabilities)

      for i in pairs(servers) do
        require("lspconfig")[i].setup({
          capabilities = capabilities,
          on_attach = on_attach,
          settings = servers[i]
        })
      end

      -- Buffer local mappings.
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = keys
      })

      -- VSCode 💡 for neovim's built-in LSP (nvim_lightbulb)
      require("nvim-lightbulb").setup({
        autocmd = { enabled = true },
        sign = {
          enabled = true,
          text = "󰌶 "
        },
      })
    end,
  },
  {
    "ray-x/lsp_signature.nvim",
    after = "nvim-lspconfig",
    opts = {
      bind = true,
      handler_opts = { border = "shadow" },
      hint_enable = false
    },
    config = function(_, opts)
      require("lsp_signature").setup(opts)
    end
  }
}
