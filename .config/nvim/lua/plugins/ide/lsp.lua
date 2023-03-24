local on_attach = function(_, bufnr)
    local keymap = function(keys, func, desc)
        if desc then
            desc = 'LSP: ' .. desc
        end

        vim.keymap.set('n', keys, func,
            { noremap = true, silent = true, buffer = bufnr, desc = desc })
    end

    keymap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
    -- `code_action_menu` plugin provides a handy pop-up menu for code action.
    -- this plugin can be used instead by `vim.lsp.buf.code_action` func
    keymap('<leader>ca', require('code_action_menu').open_code_action_menu, '[C]ode [A]ction')

    keymap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    keymap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
    keymap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
    keymap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
    keymap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
    keymap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

    -- See `:help K` for why this keymap
    keymap('K', vim.lsp.buf.hover, 'Hover Documentation')
    keymap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

    -- Lesser used LSP functionality
    keymap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
    keymap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
    keymap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
    keymap('<leader>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, '[W]orkspace [L]ist Folders')

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
        vim.lsp.buf.format()
    end, { desc = 'Format current buffer with LSP' })
end

-- nvim-cmp supports additional completion capabilities, so broadcast that
-- to servers. See https://github.com/hrsh7th/cmp-nvim-lsp/tree/59224771f91b86d1de12570b4070fe4ad7cd1eeb#capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

local servers = {
    clangd = {
        cmd = { vim.fn.exepath('clangd-mp-16'),
            '--all-scopes-completion',
            '--suggest-missing-includes',
            '--background-index',
            '--cross-file-rename',
            '--log=info',
            '--completion-style=detailed',
            '--clang-tidy',
            '--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type',
            '--fallback-style=WebKit',
            '--header-insertion=never',

            -- clangd 11+ supports reading from .clangd configuration file
            '--enable-config',

            -- store PCHs in RAM
            '--pch-storage=memory'}
    },
    gopls = {},
    lua_ls = {
        cmd = {vim.fn.exepath('lua-language-server')};
        settings = {
            Lua = {
                runtime = {
                    version = 'LuaJIT',
                    path = vim.split(package.path, ';')
                },
                diagnostics = {
                    -- Get the language server to recognize the `vim` global
                    -- globals = {'vim', 'use'},
                },
                workspace = { checkThirdParty = false },
                telemetry = {
                    enable = false,
                }
            }
        }
    },
    pyright = {
        cmd = { vim.fn.exepath('pyright-langserver'), "--pythonversion 3.11", "--stdio", }
    }
}

require 'neodev'.setup()

for i in pairs(servers) do
    if i == "clangd" then
        require 'clangd_extensions'.setup({
            server = {
                capabilities = capabilities,
                on_attach = on_attach,
                settings = servers[i]
            },
            extensions = {
                inlay_hints = {
                    only_current_line = true,
                    only_current_line_autocmd = "CursorMoved"
                }
            }
        })
    else
        require 'lspconfig'[i].setup {
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[i]
        }
    end
end
