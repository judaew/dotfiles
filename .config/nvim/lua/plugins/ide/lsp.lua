local telescope   = require("telescope.builtin")
local code_action = require("code_action_menu")

local M = {}

-- WARN: Unmap K to be able to set custom hover in M.on_attach
-- vim.keymap.set("n", "K", "")

M.on_attach = function(client, bufnr)
    local map = function(keys, func, desc)
        if desc then
            desc = "LSP: " .. desc
        end
        vim.keymap.set("n", keys, func, { desc=desc, buffer=bufnr, noremap=true})
    end

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
        vim.lsp.buf.format { async = true }
    end, { desc = "Format current buffer with LSP" })

    vim.api.nvim_buf_create_user_command(bufnr, "WorkspaceListFolders", function(_)
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, { desc = "LSP: Workspace List Folders" })

    map("<Leader>rn", vim.lsp.buf.rename,               "[R]e[n]ame")
    -- map("<Leader>ca", vim.lsp.buf.code_action,          "[C]ode [A]ction")
    map("gD",         vim.lsp.buf.declaration,          "[G]oto [D]eclaration")
    -- map("gd",         vim.lsp.buf.definition,           "[G]oto [D]efinition")
    -- map("gr",         vim.lsp.buf.references,           "[G]oto [R]eferences")
    -- map("gI",         vim.lsp.buf.implementation,       "[G]oto [I]mplementation")
    -- map("<Leader>D",  vim.lsp.buf.type_definition,      "Type [D]efinition")
    map("<Leader>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
    map("<Leader>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
    map("<Leader>wl", ":WorkspaceListFolders<CR>",         "[W]orkspace [L]ist Folders")

    -- See `:help K` for why this keymap
    map("K",          vim.lsp.buf.hover,          "Hover Documentation")
    map("<C-k>",      vim.lsp.buf.signature_help, "Signature Documentation")

    -- See commnad :Format above
    map("<Leader>fo", ":Format<CR>", "[Fo]rmat current buffer with LSP")

    --- Code Action Menu Plugin
    map("<Leader>ca", code_action.open_code_action_menu, "LSP: [C]ode [A]ction")

    --- Telescope Plugin
    map("gd",         telescope.lsp_definitions,      "[G]oto [D]efinition")
    map("gr",         telescope.lsp_references,       "[G]oto [R]eferences")
    map("gI",         telescope.lsp_implementations,  "[G]oto [I]mplementation")
    map("<Leader>D",  telescope.lsp_type_definitions, "Type [D]efinition")
    map("<Leader>ds", telescope.lsp_document_symbols, "[D]ocument [S]ymbols")
    map("<Leader>ws", telescope.lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")

    --- Inlay Hints
    -- if client.server_capabilities.inlayHintProvider then
    --     vim.lsp.inlay_hint(bufnr, true)
    -- end

    if client.server_capabilities.inlayHintProvider then
        vim.api.nvim_create_augroup("lsp_augroup", { clear = true })

        vim.api.nvim_create_autocmd("InsertEnter", {
            buffer = bufnr,
            callback = function() vim.lsp.inlay_hint(bufnr, true) end,
            group = "lsp_augroup",
        })
        vim.api.nvim_create_autocmd("InsertLeave", {
            buffer = bufnr,
            callback = function() vim.lsp.inlay_hint(bufnr, false) end,
            group = "lsp_augroup",
        })
    end

    --- Highlight current symbol
    -- vim.cmd([[hi! link LspReferenceText CursorColumn]])
    -- vim.cmd([[hi! link LspReferenceRead CursorColumn]])
    -- vim.cmd([[hi! link LspReferenceWrite CursorColumn]])
    -- vim.cmd([[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]])
    -- vim.cmd([[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]])
    -- vim.cmd([[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]])
end

M.capabilities = vim.lsp.protocol.make_client_capabilities()
-- See https://github.com/nvim-lua/completion-nvim/issues/258
M.capabilities.textDocument.completion.completionItem.snippetSupport = true
-- nvim-cmp supports additional completion capabilities, so broadcast that
-- to servers. See https://github.com/hrsh7th/cmp-nvim-lsp/tree/59224771f91b86d1de12570b4070fe4ad7cd1eeb#capabilities
M.capabilities = require("cmp_nvim_lsp").default_capabilities(M.capabilities)

M.servers = {
    clangd = {
        init_options = {
            clangdFileStatus = true,
        },
        cmd = { vim.fn.exepath("clangd-mp-16"),
            "--all-scopes-completion",
            "--suggest-missing-includes",
            "--background-index",
            "--cross-file-rename",
            "--log=info",
            "--completion-style=detailed",
            "--clang-tidy",
            "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type",
            "--fallback-style=WebKit",
            "--header-insertion=never",

            -- clangd 11+ supports reading from .clangd configuration file
            "--enable-config",

            -- store PCHs in RAM
            "--pch-storage=memory"
        }
    },
    gopls = {},
    lua_ls = {
        cmd = { vim.fn.exepath("lua-language-server") },
        settings = {
            Lua = {
                runtime = {
                    version = "LuaJIT",
                },
                hint = {
                    enable = true
                },
                workspace = { checkThirdParty = true },
                telemetry = {
                    enable = false,
                },
                maxPreload = 500, -- 500 KB or ~10k lines per file
                preloadFileSize = 500 -- 500 KB or ~10k lines per file
            }
        }
    },
    pylsp = {},
    bashls = {
        cmd = { vim.fn.exepath("bash-language-server"), "start" }
    },
}

function M.lsp()
    for i in pairs(M.servers) do
        require("lspconfig")[i].setup({
            capabilities = M.capabilities,
            on_attach = M.on_attach,
            settings = M.servers[i]
        })
    end
end

-- VSCode 💡 for neovim's built-in LSP
function M.nvim_lightbulb()
    require("nvim-lightbulb").setup({
        autocmd = { enabled = true },
        sign = { enabled = true },
    })
end

return M
