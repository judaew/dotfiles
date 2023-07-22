local key         = require("utils.keymap")
local telescope   = require("telescope.builtin")
local code_action = require("code_action_menu")

local M = {}

local on_attach = function(_, bufnr)
    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
        vim.lsp.buf.format { async = true }
    end, { desc = "Format current buffer with LSP" })

    local keymaps_table = {
        { "<Leader>rn", vim.lsp.buf.rename, "[R]e[n]ame" },
        -- The `code_action_menu` plugin provides a handy pop-up menu for code
        -- action. This plugin can be used instead by `vim.lsp.buf.code_action`
        -- func.
        { "<Leader>ca", code_action.open_code_action_menu, "[C]ode [A]ction" },
        { "gD",         vim.lsp.buf.declaration,           "[G]oto [D]eclaration" },
        { "gd",         vim.lsp.buf.definition,            "[G]oto [D]efinition" },
        -- The `lsp_references` func from Telescope provides "Goto References"
        -- func in a float window. This plugin  can be used insstead by
        -- `vim.lsp.buf.references` func.
        { "gr",         telescope.lsp_references,                "[G]oto [R]eferences" },
        { "gI",         vim.lsp.buf.implementation,              "[G]oto [I]mplementation" },
        { "<Leader>D",  vim.lsp.buf.type_definition,             "Type [D]efinition" },
        { "<Leader>ds", telescope.lsp_document_symbols,          "[D]ocument [S]ymbols" },
        { "<Leader>ws", telescope.lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols" },
        { "<Leader>wa", vim.lsp.buf.add_workspace_folder,        "[W]orkspace [A]dd Folder" },
        { "<Leader>wr", vim.lsp.buf.remove_workspace_folder,     "[W]orkspace [R]emove Folder" },
        { "<Leader>wl", function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
            "[W]orkspace [L]ist Folders" },

        -- See `:help K` for why this keymap
        { "K",          vim.lsp.buf.hover,          "Hover Documentation" },
        { "<C-k>",      vim.lsp.buf.signature_help, "Signature Documentation" },

        -- See commnad :Format above
        { "<Leader>fo", ":Format<CR>", "[Fo]rmat current buffer with LSP" }
    }
    key.bulk_set(keymaps_table, "n", "LSP: ", "buffer = bufnr")
end

M.capabilities = vim.lsp.protocol.make_client_capabilities()
-- See https://github.com/nvim-lua/completion-nvim/issues/258
M.capabilities.textDocument.completion.completionItem.snippetSupport = true
-- nvim-cmp supports additional completion capabilities, so broadcast that
-- to servers. See https://github.com/hrsh7th/cmp-nvim-lsp/tree/59224771f91b86d1de12570b4070fe4ad7cd1eeb#capabilities
M.capabilities = require("cmp_nvim_lsp").default_capabilities(M.capabilities)

local servers = {
    clangd = {
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
            "--pch-storage=memory",
        },
    },
    gopls = {},
    lua_ls = {
        cmd = { vim.fn.exepath("lua-language-server") },
        settings = {
            Lua = {
                runtime = {
                    version = "LuaJIT",
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
    for i in pairs(servers) do
        if i == "clangd" then
            require("clangd_extensions").setup({
                server = {
                    capabilities = M.capabilities,
                    on_attach = on_attach,
                    settings = servers[i]
                },
                extensions = {
                    autoSetHints = true,
                }
            })
        else
            require("lspconfig")[i].setup({
                capabilities = M.capabilities,
                on_attach = on_attach,
                settings = servers[i]
            })
        end
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
