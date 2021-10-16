local lspconfig = require'lspconfig'
local cmp_nvim_lsp = require'cmp_nvim_lsp'

lspconfig.clangd.setup {
    capabilities = cmp_nvim_lsp.update_capabilities(vim.lsp.protocol.make_client_capabilities());
    cmd = { "clangd-mp-12", "--background-index" };
}

lspconfig.sumneko_lua.setup {
    cmd = {"/opt/local/bin/lua-language-server"};
    settings = {
        Lua = {
            runtime = {
                version = 'LuaJIT',
                path = vim.split(package.path, ';')
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {'vim'},
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {[vim.fn.expand('$VIMRUNTIME/lua')] = true}
            },
            telemetry = {
                enable = false,
            }
        }
    }
}
